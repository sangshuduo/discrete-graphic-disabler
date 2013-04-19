/**
 *
 * Disable discrete graphics (currently Nvidia only)
 *
 * Base on Bumblebee bbswitch
 *
 * Usage:
 * Disable discrete graphics when loaded
 * Restore the former state when unloaded
 *
 */

#define pr_fmt(fmt) KBUILD_MODNAME ": " fmt

#include <linux/pci.h>
#include <linux/acpi.h>
#include <linux/module.h>
#include <asm/uaccess.h>
#include <linux/suspend.h>
#include <linux/pm_runtime.h>

#define DISCRETE_DISABLER_VERSION "0.1"

enum {
	GPU_UNCHANGED = -1,
	GPU_DISABLE = 0,
	GPU_ENABLE = 1
};

static int device_vendor = 0xffff;
static int discrete_state = GPU_UNCHANGED;
static int load_discrete_state = GPU_UNCHANGED;

struct mutex discrete_lock;

static const char acpi_optimus_dsm_muid[16] = {
	0xF8, 0xD8, 0x86, 0xA4, 0xDA, 0x0B, 0x1B, 0x47,
	0xA7, 0x2B, 0x60, 0x42, 0xA6, 0xB5, 0xBE, 0xE0,
};

static const char acpi_nvidia_dsm_muid[16] = {
	0xA0, 0xA0, 0x95, 0x9D, 0x60, 0x00, 0x48, 0x4D,
	0xB3, 0x4D, 0x7E, 0x5F, 0xEA, 0x12, 0x9F, 0xD4
};

/*
  The next UUID has been found as well in
  https://bugs.launchpad.net/lpbugreporter/+bug/752542:

  0xD3, 0x73, 0xD8, 0x7E, 0xD0, 0xC2, 0x4F, 0x4E,
  0xA8, 0x54, 0x0F, 0x13, 0x17, 0xB0, 0x1C, 0x2C 
  It looks like something for Intel GPU:
  http://lxr.linux.no/#linux+v3.1.5/drivers/gpu/drm/i915/intel_acpi.c
*/

#define DSM_TYPE_UNSUPPORTED    0
#define DSM_TYPE_OPTIMUS        1
#define DSM_TYPE_NVIDIA         2
static int dsm_type = DSM_TYPE_UNSUPPORTED;

static struct pci_dev *dis_dev;
static acpi_handle dis_handle;

/* used for keeping the PM event handler */
static struct notifier_block nb;

// Return GPU_DISABLE if disabled, GPU_ENABLE if enabled
static int get_discrete_state(void) {
	u32 cfg_word;
	/* read first config word which contains Vendor and Device ID, 
	 * if all bits are 1, the device is assuemd to be off, 
	 * if one of the bits is not 1, the device is on.
	 */
	pci_read_config_dword(dis_dev, 0, &cfg_word);
	
	if(likely(~cfg_word))
		return GPU_ENABLE;
	else
		return GPU_DISABLE;
}

static char *buffer_to_string(const char *buffer, size_t n, char *target) {
	int i;
	for (i=0; i<n; i++) {
		snprintf(target + i * 5, 5 * (n - i), "0x%02X,", buffer ? buffer[i] & 0xFF : 0);
	}
	return target;
}

// Returns 0 if the call succeeded and non-zero otherwise. If the call
// succeeded, the result is stored in "result" providing that the result is an
// integer or a buffer containing 4 values
static int acpi_call_dsm(acpi_handle handle, const char muid[16], int revid,
			 int func, char args[4], uint32_t *result) {
	struct acpi_buffer output = { ACPI_ALLOCATE_BUFFER, NULL };
	struct acpi_object_list input;
	union acpi_object params[4];
	union acpi_object *obj;
	int err;

	input.count = 4;
	input.pointer = params;
	params[0].type = ACPI_TYPE_BUFFER;
	params[0].buffer.length = 16;
	params[0].buffer.pointer = (char *)muid;
	params[1].type = ACPI_TYPE_INTEGER;
	params[1].integer.value = revid;
	params[2].type = ACPI_TYPE_INTEGER;
	params[2].integer.value = func;
	params[3].type = ACPI_TYPE_BUFFER;
	params[3].buffer.length = 4;
	if (args) {
		params[3].buffer.pointer = args;
	} else {
		// Some implementations (Asus U36SD) seem to check the args before the
		// function ID and crash if it is not a buffer.
		params[3].buffer.pointer = (char[4]){0, 0, 0, 0};
	}

	err = acpi_evaluate_object(handle, "_DSM", &input, &output);
	if (err) {
		char muid_str[5 * 16];
		char args_str[5 * 4];

		pr_warn("failed to evaluate _DSM {%s} 0x%X 0x%X {%s}: %s\n",
			buffer_to_string(muid, 16, muid_str), revid, func,
			buffer_to_string(args,  4, args_str), acpi_format_exception(err));
		return err;
	}

	obj = (union acpi_object *)output.pointer;

	if (obj->type == ACPI_TYPE_INTEGER && result) {
		*result = obj->integer.value;
	} else if (obj->type == ACPI_TYPE_BUFFER) {
		if (obj->buffer.length == 4 && result) {
			*result = 0;
			*result |= obj->buffer.pointer[0];
			*result |= (obj->buffer.pointer[1] << 8);
			*result |= (obj->buffer.pointer[2] << 16);
			*result |= (obj->buffer.pointer[3] << 24);
		}
	} else {
		pr_warn("_DSM call yields an unsupported result type: %#x\n",
			obj->type);
	}

	kfree(output.pointer);
	return 0;
}

// Returns 1 if a _DSM function and its function index exists and 0 otherwise
static int has_dsm_func(const char muid[16], int revid, int sfnc) {
	u32 result = 0;

	// fail if the _DSM call failed
	if (acpi_call_dsm(dis_handle, muid, revid, 0, 0, &result))
		return 0;

	// ACPI Spec v4 9.14.1: if bit 0 is zero, no function is supported. If
	// the n-th bit is enabled, function n is supported
	return result & 1 && result & (1 << sfnc);
}

/* power bus so we can read PCI configuration space */
static void dis_dev_get(void) {
	if (dis_dev->bus && dis_dev->bus->self)
		pm_runtime_get_sync(&dis_dev->bus->self->dev);
}

static void dis_dev_put(void) {
	if (dis_dev->bus && dis_dev->bus->self)
		pm_runtime_put_sync(&dis_dev->bus->self->dev);
}

static int optimus_acpi_dsm(void) {
	if (dsm_type == DSM_TYPE_OPTIMUS) {
		char args[] = {1, 0, 0, 3};
		u32 result = 0;

		if (acpi_call_dsm(dis_handle, acpi_optimus_dsm_muid, 0x100, 0x1A, args,
				  &result)) {
			// failure
			return 1;
		}
		pr_debug("Result of Optimus _DSM call: %08X\n", result);
	}
	return 0;
}

static int nvidia_acpi_off(void) {
	if (dsm_type == DSM_TYPE_NVIDIA) {
		char args[] = {2, 0, 0, 0};
		u32 result = 0;

		if (acpi_call_dsm(dis_handle, acpi_nvidia_dsm_muid, 0x102, 0x3, args,
				  &result)) {
			// failure
			return 1;
		}
		pr_debug("Result of _DSM call for OFF: %08X\n", result);
	}
	return 0;
}

static void radeon_off(void) {
	int i;
	struct acpi_object_list arg;

	pr_info("turn radeon off\n");
	if (unlikely(get_discrete_state() == GPU_DISABLE)) {
		return;
	}

	// to prevent the system from possibly locking up, don't disable the device
	// if it's still in use by a driver (i.e. nouveau or nvidia)
	if(dis_dev->driver) {
		pr_warn("device %s is in use by driver '%s', refusing OFF\n",
			dev_name(&dis_dev->dev), dis_dev->driver->name);
		return;
	}

	pr_info("disabling discrete grapics\n");

	arg.count = 0;
	arg.pointer = NULL;
	acpi_evaluate_object(dis_handle, "_OFF", &arg, NULL);

	pci_save_state(dis_dev);
	pci_clear_master(dis_dev);
	
	mutex_lock(&discrete_lock);

	pci_disable_device(dis_dev);
	for(i=0; i<16; i++) {
		pci_set_power_state(dis_dev, PCI_D3cold);
		cpu_relax();
	}

	mutex_unlock(&discrete_lock);

	discrete_state = GPU_DISABLE;
}

static void optimus_off(void) {
	if (unlikely(get_discrete_state() == GPU_DISABLE)) {
		return;
	}

	// to prevent the system from possibly locking up, don't disable the device
	// if it's still in use by a driver (i.e. nouveau or nvidia)
	if (dis_dev->driver) {
		pr_warn("device %s is in use by driver '%s', refusing OFF\n",
			dev_name(&dis_dev->dev), dis_dev->driver->name);
		return;
	}

	pr_info("disabling discrete grapics\n");

	if (optimus_acpi_dsm()) {
		pr_warn("Optimus ACPI call failed, the device is not disabled.\n");
		return;
	}

	pci_save_state(dis_dev);
	pci_clear_master(dis_dev);
	pci_disable_device(dis_dev);
	pci_set_power_state(dis_dev, PCI_D3cold);

	if (nvidia_acpi_off())
		pr_warn("The discrete card could not be disabled by a _DSM call\n");

	discrete_state = GPU_DISABLE;
}

static int nvidia_acpi_on(void) {
	if (dsm_type == DSM_TYPE_NVIDIA) {
		char args[] = {1, 0, 0, 0};
		u32 result = 0;

		if (acpi_call_dsm(dis_handle, acpi_nvidia_dsm_muid, 0x102, 0x3, args,
				  &result)) {
			// failure
			return 1;
		}
		pr_debug("Result of _DSM call for ON: %08X\n", result);
	}
	return 0;
}

static void radeon_on(void) {
	struct acpi_object_list arg;

	pr_info("turn radeon on\n");
	if (unlikely(get_discrete_state() == GPU_ENABLE))
		return;

	pr_info("enabling discrete graphics\n");

	arg.count = 0;
	arg.pointer = NULL;
	acpi_evaluate_object(dis_handle, "_ON", &arg, NULL);

	pci_set_power_state(dis_dev, PCI_D0);
	pci_restore_state(dis_dev);
	if (pci_enable_device(dis_dev))
		pr_warn("failed to enable %s\n", dev_name(&dis_dev->dev));
	pci_set_master(dis_dev);

	discrete_state = GPU_ENABLE;
}

static void optimus_on(void) {
	if (unlikely(get_discrete_state() == GPU_ENABLE))
		return;

	pr_info("enabling discrete graphics\n");

	if (nvidia_acpi_on())
		pr_warn("The discrete card could not be enabled by a _DSM call\n");

	pci_set_power_state(dis_dev, PCI_D0);
	pci_restore_state(dis_dev);
	if (pci_enable_device(dis_dev))
		pr_warn("failed to enable %s\n", dev_name(&dis_dev->dev));
	pci_set_master(dis_dev);

	discrete_state = GPU_ENABLE;
}

static void discrete_on(void) {
	if (device_vendor == PCI_VENDOR_ID_NVIDIA)
		optimus_on();
	else
		radeon_on();
}

static void discrete_off(void) {
	if (device_vendor == PCI_VENDOR_ID_NVIDIA)
		optimus_off();
	else
		radeon_off();
}

static int discrete_pm_handler(struct notifier_block *nbp,
			      unsigned long event_type, void *p) {
	switch (event_type) {
	case PM_HIBERNATION_PREPARE:
	case PM_SUSPEND_PREPARE:
		// enable the device before suspend to avoid the PCI config space from
		// being saved incorrectly
		if (load_discrete_state == GPU_ENABLE &&
		    discrete_state == GPU_DISABLE) {
			dis_dev_get();
			discrete_on();
			dis_dev_put();
		}
		break;
	case PM_POST_HIBERNATION:
	case PM_POST_SUSPEND:
	case PM_POST_RESTORE:
		// after suspend, the card is on, but if it was off before suspend,
		// disable it again
		if (load_discrete_state == GPU_ENABLE &&
		    discrete_state == GPU_ENABLE) {
			dis_dev_get();
			discrete_off();
			dis_dev_put();
		}
		break;
	case PM_RESTORE_PREPARE:
		// deliberately don't do anything as it does not occur before suspend
		// nor hibernate, but before restoring a saved image. In that case,
		// either PM_POST_HIBERNATION or PM_POST_RESTORE will be called
		break;
	}
	return 0;
}
	   
static int __init discrete_disabler_init(void)
{
	struct pci_dev *pdev = NULL;
	struct pci_dev *igd_dev = NULL;
	acpi_handle igd_handle = NULL;

	pr_info("version %s\n", DISCRETE_DISABLER_VERSION);

	while ((pdev = pci_get_device(PCI_ANY_ID, PCI_ANY_ID, pdev)) != NULL) {
		acpi_handle handle;
		struct acpi_buffer buf = { ACPI_ALLOCATE_BUFFER, NULL};
		int pci_class = pdev->class >> 8;

		if (pci_class != PCI_CLASS_DISPLAY_VGA && 
			pci_class != PCI_CLASS_DISPLAY_3D &&
			pci_class != PCI_CLASS_DISPLAY_OTHER)
			continue;

		handle = DEVICE_ACPI_HANDLE(&pdev->dev);
		
		if (!handle) {
			pr_warn("can not find ACPI handle for VGA device %s\n",
				dev_name(&pdev->dev));
			continue;
		}

		acpi_get_name(handle, ACPI_FULL_PATHNAME, &buf);

		if (pdev->vendor == PCI_VENDOR_ID_INTEL) {
			igd_dev = pdev;
			igd_handle = handle;
			pr_info("Found intergrated VGA device %s: %s\n",
				dev_name(&pdev->dev), (char *)buf.pointer);
		} else {
			dis_dev = pdev;
			dis_handle = handle;
			pr_info("Found discrete VGA device %s: %s\n",
				dev_name(&pdev->dev), (char *)buf.pointer);
		}

		kfree(buf.pointer);

		// If both intergrated and discrete VGA devices are found, we can break now.
		if(unlikely(igd_handle && dis_handle)) {
			break;
		}
	}
	
	if (!dis_dev) {
		pr_err("No discrete VGA device found!\n");
		return -ENODEV;
	}

	device_vendor = dis_dev->vendor;

	if (device_vendor == PCI_VENDOR_ID_NVIDIA) {
		if (has_dsm_func(acpi_optimus_dsm_muid, 0x100, 0x1A)) {
			dsm_type = DSM_TYPE_OPTIMUS;
			pr_info("detected an Optimus _DSM function\n");
		} else if (has_dsm_func(acpi_nvidia_dsm_muid, 0x102, 0x3)) {
			dsm_type = DSM_TYPE_NVIDIA;
			pr_info("detected a nVidia _DSM function\n");
		} else {
			/* At least two Acer machines are known to use the intel ACPI handle
			 * with the legacy nvidia DSM */
			dis_dev = igd_dev;
			dis_handle = igd_handle;
			if (has_dsm_func(acpi_nvidia_dsm_muid, 0x102, 0x3)) {
				dsm_type = DSM_TYPE_NVIDIA;
				pr_info("detected a nVidia _DSM function on the"
					" integrated video card\n");
			} else {
				pr_err("No suitable _DSM call found.\n");
				return -ENODEV;
			}
		}
	} else if (device_vendor == PCI_VENDOR_ID_ATI) {
		pr_info("detected an ATI discrete card");
	}

	pr_info("Successfully loaded. Discrete card found: %s\n",
		dev_name(&dis_dev->dev));

	mutex_init(&discrete_lock);

	dis_dev_get();
	load_discrete_state = get_discrete_state();
	
	if (unlikely(load_discrete_state == GPU_DISABLE)) {
		pr_info("Discrete card %s is off. Do nothing.",
			dev_name(&dis_dev->dev));
		goto nothing;
	} else {
		discrete_off();
		pr_info("Successfully disable discrete card %s\n",
			dev_name(&dis_dev->dev));
	}
	
	nb.notifier_call = &discrete_pm_handler;
	register_pm_notifier(&nb);

nothing:dis_dev_put();
	return 0;
}

static void __exit discrete_disabler_exit(void)
{
	if (!dis_dev || !dis_handle) {
		return;
	}

	if (unlikely(load_discrete_state == GPU_DISABLE)) {
		pr_info("discrete card %s is disabled before module load. Do nothing.\n",
			dev_name(&dis_dev->dev));
		return;
	}

	mutex_destroy(&discrete_lock);

	dis_dev_get();
	
	if (discrete_state == GPU_ENABLE) {
		pr_info("discrete card %s has already been enabled. Do nothing.\n",
			dev_name(&dis_dev->dev));
		return;
	}

	discrete_on();

	pr_info("Unloaded and Discrete card %s is enabled.\n",
		dev_name(&dis_dev->dev));
	
	dis_dev_put();
	
	if (nb.notifier_call)
		unregister_pm_notifier(&nb);
}

module_init(discrete_disabler_init);
module_exit(discrete_disabler_exit);

static struct pci_device_id discrete_pci_table[] = {
	{
		PCI_DEVICE(PCI_VENDOR_ID_NVIDIA, PCI_ANY_ID),
		.class = PCI_BASE_CLASS_DISPLAY << 16,
		.class_mask  = 0xff << 16,
	},
	{
		PCI_DEVICE(PCI_VENDOR_ID_NVIDIA_SGS, PCI_ANY_ID),
		.class = PCI_BASE_CLASS_DISPLAY << 16,
		.class_mask  = 0xff << 16,
	},
	{
		PCI_DEVICE(PCI_VENDOR_ID_ATI, PCI_ANY_ID),
		.class = PCI_BASE_CLASS_DISPLAY << 16,
		.class_mask  = 0xff << 16,
	},
	{}
};

MODULE_DEVICE_TABLE(pci, discrete_pci_table);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Yang Bai <hamo@canonical.com>");
MODULE_AUTHOR("Shuduo Sang <shuduo.sang@canonical.com>");
MODULE_VERSION(DISCRETE_DISABLER_VERSION);


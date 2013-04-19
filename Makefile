# needless plumbing necessary for dkms to function correctly
obj-m := discrete-graphic-disabler.o


path := $(shell pwd)

all:
	make -C /lib/modules/$(shell uname -r)/build M=$(path) modules

clean:
	make -C /lib/modules/$(shell uname -r)/build M=$(path) clean

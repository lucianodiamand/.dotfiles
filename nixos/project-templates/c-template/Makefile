AS		= $(CROSS_COMPILE)as
AR		= $(CROSS_COMPILE)ar
CC		= $(CROSS_COMPILE)gcc
CPP		= $(CC) -E
LD		= $(CROSS_COMPILE)ld
NM		= $(CROSS_COMPILE)nm
OBJCOPY		= $(CROSS_COMPILE)objcopy
OBJDUMP		= $(CORSS_COMPILE)objdump
RANLIB		= $(CROSS_COMPILE)ranlib
READELF		= $(CROSS_COMPILE)readelf
SIZE		= $(CROSS_COMPILE)size
STRINGS		= $(CROSS_COMPILE)strings
STRIP		= $(CROSS_COMPILE)strip

export AS AR CC CPP LD NM OBJCOPY OBJDUMP RANLIB \
	READELF SIZE STRINGS STRIP

CFLAGS		= -O2 -Wall
HEADER_OPS	=
LDFLAGS		=

EXEC_NAME	= hello_command
INSTALL		= install
INSTALL_DIR	= ${PROJECT_ROOT}/rootfs/bin

CFILES		= hello.c
HFILES		=

OBJS		= $(CFILES:%.c=%.o)

all: hello

.c.o:
	$(CC) $(CFLAGS) $(HEADER_OPS) -c $<

hello: $(OBJS) $(HFILES)
	$(CC) -o $(EXEC_NAME) $ˆ $(LDFLAGS)

install: hello
	test -d $(INSTALL_DIR) || $(INSTALL) -d -m 755 $
(INSTALL_DIR)
	$(INSTALL) -m 755 $(EXEC_NAME) $(INSTALL_DIR)

clean:
	rm -f *.o $(EXEC_NAME)


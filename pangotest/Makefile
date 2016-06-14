CC ?= gcc
PKGCONFIG = $(shell which pkg-config)
CFLAGS = $(shell $(PKGCONFIG) --cflags gtk+-3.0)
LIBS = $(shell $(PKGCONFIG) --libs gtk+-3.0)
GLIB_COMPILE_RESOURCES = $(shell $(PKGCONFIG) --variable=glib_compile_resources gio-2.0)
GLIB_COMPILE_SCHEMAS = $(shell $(PKGCONFIG) --variable=glib_compile_schemas gio-2.0)

SRC = cairo.c gtk.c

OBJS = $(BUILT_SRC:.c=.o) $(SRC:.c=.o)

all: cairo gtk

%.o: %.c
	$(CC) -g -c -o $(@F) $(CFLAGS) $<

cairo: $(OBJS) 
	$(CC) -o $(@F) $(LIBS) cairo.o

gtk: $(OBJS) 
	$(CC) -o $(@F) $(LIBS) gtk.o

clean:
	rm -f $(OBJS)
	rm -f test

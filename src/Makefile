# $Revision: 1.12 $

###  You need a C compiler that compiles ANSI C code.
CC      = gcc
CFLAGS  = -Wall -pedantic -O

###  If you need additional linker flags add them here.
LDFLAGS =

###  The directory where the Elk installation resides on your system.
ELKDIR  = /usr/elk

###  Additional libraries.  You may want to insert the output of the
###  shell-script $(ELKDIR)/lib/ldflags here.
LIBS    = -lm

###  The makedepend program (it's usually installed with the X11 binaries).
MAKEDEP = makedepend

###  The directory under which you will install the Scheme files.
DIR = /usr/local/lib/unroff

###  The default output format.
FORMAT = html




###  End of configurable variables.
###  ------------------------------------------------------------------------- 

SHELL   = /bin/sh
INCLUDE = -I$(ELKDIR)/include
ELK     = $(ELKDIR)/lib/module.o
DEFS    = -DDEFAULT_DIR=\"$(DIR)\" -DDEFAULT_FORMAT=\"$(FORMAT)\"
CTAGS   = ctags -t -w

SOURCES	= \
	args.c\
	buffer.c\
	error.c\
	event.c\
	expr.c\
	gcroot.c\
	insert.c\
	malloc.c\
	parse.c\
	prim.c\
	scmtable.c\
	stream.c\
	subst.c\
	table.c\
	unroff.c

OBJECTS	= \
	args.o\
	buffer.o\
	error.o\
	event.o\
	expr.o\
	gcroot.o\
	insert.o\
	malloc.o\
	parse.o\
	prim.o\
	scmtable.o\
	stream.o\
	subst.o\
	table.o\
	unroff.o\
	$(ELK)

ALL	= unroff

all:    $(ALL)

.c.o:
	$(CC) $(CFLAGS) $(INCLUDE) $(DEFS) -c $<

clean:
	rm -f *.o $(ALL)

tags ctags:    $(SOURCES)
	$(CTAGS) $(SOURCES)

unroff:  $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LIBS)

depend:    Makefile $(SOURCES)
	$(MAKEDEP) $(INCLUDE) $(SOURCES)


# DO NOT DELETE THIS LINE -- make depend depends on it.

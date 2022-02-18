# $Revision: 1.12 $

###  You need a C compiler that compiles ANSI C code.
CC      = gcc
CFLAGS  = -Wall -pedantic -O -I/usr/include/x86_64-linux-gnu

###  If you need additional linker flags add them here.
LDFLAGS =

###  The directory where the Elk installation resides on your system.
ELKDIR  = /usr/local/share/elk

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
INCLUDE = -I$(ELKDIR)/include -I/usr/include/x86_64-linux-gnu -I/usr/include/linux -I/usr/lib/gcc/x86_64-linux-gnu/11/include -I/usr/local/include/elk
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

args.o: unroff.h /usr/include/assert.h /usr/include/features.h
args.o: /usr/include/features-time64.h
args.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
args.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
args.o: /usr/include/stdc-predef.h /usr/include/x86_64-linux-gnu/sys/cdefs.h
args.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
args.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h /usr/include/linux/limits.h
args.o: /usr/include/stdlib.h
args.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
args.o: /usr/include/linux/stddef.h
args.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
args.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
args.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
args.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
args.o: /usr/include/x86_64-linux-gnu/sys/types.h
args.o: /usr/include/x86_64-linux-gnu/bits/types.h
args.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
args.o: /usr/include/x86_64-linux-gnu/bits/time64.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
args.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
args.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
args.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
args.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
args.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
args.o: /usr/include/x86_64-linux-gnu/sys/select.h
args.o: /usr/include/x86_64-linux-gnu/bits/select.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
args.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
args.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
args.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
args.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
args.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
args.o: /usr/include/alloca.h
args.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
args.o: /usr/include/stdio.h
args.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
args.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
args.o: /usr/include/linux/string.h /usr/include/linux/errno.h
args.o: /usr/include/x86_64-linux-gnu/asm/errno.h
args.o: /usr/include/asm-generic/errno.h
args.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
args.o: /usr/include/linux/time.h /usr/include/linux/types.h
args.o: /usr/include/x86_64-linux-gnu/asm/types.h
args.o: /usr/include/asm-generic/types.h /usr/include/asm-generic/int-ll64.h
args.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
args.o: /usr/include/asm-generic/bitsperlong.h
args.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
args.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
args.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
args.o: /usr/include/asm-generic/posix_types.h
args.o: /usr/include/linux/time_types.h /usr/include/math.h
args.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
args.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
args.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
args.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
args.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
args.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
args.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
args.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
args.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
args.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
args.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
args.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
args.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
args.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
args.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
args.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
args.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
args.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
args.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
args.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
args.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
args.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
args.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
buffer.o: unroff.h /usr/include/assert.h /usr/include/features.h
buffer.o: /usr/include/features-time64.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
buffer.o: /usr/include/stdc-predef.h
buffer.o: /usr/include/x86_64-linux-gnu/sys/cdefs.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
buffer.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
buffer.o: /usr/include/linux/limits.h /usr/include/stdlib.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
buffer.o: /usr/include/linux/stddef.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
buffer.o: /usr/include/x86_64-linux-gnu/sys/types.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/time64.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
buffer.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
buffer.o: /usr/include/x86_64-linux-gnu/sys/select.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/select.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
buffer.o: /usr/include/alloca.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
buffer.o: /usr/include/stdio.h
buffer.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
buffer.o: /usr/include/linux/string.h /usr/include/linux/errno.h
buffer.o: /usr/include/x86_64-linux-gnu/asm/errno.h
buffer.o: /usr/include/asm-generic/errno.h
buffer.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
buffer.o: /usr/include/linux/time.h /usr/include/linux/types.h
buffer.o: /usr/include/x86_64-linux-gnu/asm/types.h
buffer.o: /usr/include/asm-generic/types.h
buffer.o: /usr/include/asm-generic/int-ll64.h
buffer.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
buffer.o: /usr/include/asm-generic/bitsperlong.h
buffer.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
buffer.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
buffer.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
buffer.o: /usr/include/asm-generic/posix_types.h
buffer.o: /usr/include/linux/time_types.h /usr/include/math.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
buffer.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
buffer.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
buffer.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
buffer.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
buffer.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
buffer.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
buffer.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
buffer.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
buffer.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
buffer.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
buffer.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
buffer.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
buffer.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
error.o: unroff.h /usr/include/assert.h /usr/include/features.h
error.o: /usr/include/features-time64.h
error.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
error.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
error.o: /usr/include/stdc-predef.h /usr/include/x86_64-linux-gnu/sys/cdefs.h
error.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
error.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
error.o: /usr/include/linux/limits.h /usr/include/stdlib.h
error.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
error.o: /usr/include/linux/stddef.h
error.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
error.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
error.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
error.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
error.o: /usr/include/x86_64-linux-gnu/sys/types.h
error.o: /usr/include/x86_64-linux-gnu/bits/types.h
error.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
error.o: /usr/include/x86_64-linux-gnu/bits/time64.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
error.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
error.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
error.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
error.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
error.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
error.o: /usr/include/x86_64-linux-gnu/sys/select.h
error.o: /usr/include/x86_64-linux-gnu/bits/select.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
error.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
error.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
error.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
error.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
error.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
error.o: /usr/include/alloca.h
error.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
error.o: /usr/include/stdio.h
error.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
error.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
error.o: /usr/include/linux/string.h /usr/include/linux/errno.h
error.o: /usr/include/x86_64-linux-gnu/asm/errno.h
error.o: /usr/include/asm-generic/errno.h
error.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
error.o: /usr/include/linux/time.h /usr/include/linux/types.h
error.o: /usr/include/x86_64-linux-gnu/asm/types.h
error.o: /usr/include/asm-generic/types.h /usr/include/asm-generic/int-ll64.h
error.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
error.o: /usr/include/asm-generic/bitsperlong.h
error.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
error.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
error.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
error.o: /usr/include/asm-generic/posix_types.h
error.o: /usr/include/linux/time_types.h /usr/include/math.h
error.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
error.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
error.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
error.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
error.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
error.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
error.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
error.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
error.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
error.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
error.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
error.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
error.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
error.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
error.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
error.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
error.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
error.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
error.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
error.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
error.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
error.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
error.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
event.o: unroff.h /usr/include/assert.h /usr/include/features.h
event.o: /usr/include/features-time64.h
event.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
event.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
event.o: /usr/include/stdc-predef.h /usr/include/x86_64-linux-gnu/sys/cdefs.h
event.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
event.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
event.o: /usr/include/linux/limits.h /usr/include/stdlib.h
event.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
event.o: /usr/include/linux/stddef.h
event.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
event.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
event.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
event.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
event.o: /usr/include/x86_64-linux-gnu/sys/types.h
event.o: /usr/include/x86_64-linux-gnu/bits/types.h
event.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
event.o: /usr/include/x86_64-linux-gnu/bits/time64.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
event.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
event.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
event.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
event.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
event.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
event.o: /usr/include/x86_64-linux-gnu/sys/select.h
event.o: /usr/include/x86_64-linux-gnu/bits/select.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
event.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
event.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
event.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
event.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
event.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
event.o: /usr/include/alloca.h
event.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
event.o: /usr/include/stdio.h
event.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
event.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
event.o: /usr/include/linux/string.h /usr/include/linux/errno.h
event.o: /usr/include/x86_64-linux-gnu/asm/errno.h
event.o: /usr/include/asm-generic/errno.h
event.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
event.o: /usr/include/linux/time.h /usr/include/linux/types.h
event.o: /usr/include/x86_64-linux-gnu/asm/types.h
event.o: /usr/include/asm-generic/types.h /usr/include/asm-generic/int-ll64.h
event.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
event.o: /usr/include/asm-generic/bitsperlong.h
event.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
event.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
event.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
event.o: /usr/include/asm-generic/posix_types.h
event.o: /usr/include/linux/time_types.h /usr/include/math.h
event.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
event.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
event.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
event.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
event.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
event.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
event.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
event.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
event.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
event.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
event.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
event.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
event.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
event.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
event.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
event.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
event.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
event.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
event.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
event.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
event.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
event.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
event.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
expr.o: unroff.h /usr/include/assert.h /usr/include/features.h
expr.o: /usr/include/features-time64.h
expr.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
expr.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
expr.o: /usr/include/stdc-predef.h /usr/include/x86_64-linux-gnu/sys/cdefs.h
expr.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
expr.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h /usr/include/linux/limits.h
expr.o: /usr/include/stdlib.h
expr.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
expr.o: /usr/include/linux/stddef.h
expr.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
expr.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
expr.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
expr.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
expr.o: /usr/include/x86_64-linux-gnu/sys/types.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types.h
expr.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
expr.o: /usr/include/x86_64-linux-gnu/bits/time64.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
expr.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
expr.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
expr.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
expr.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
expr.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
expr.o: /usr/include/x86_64-linux-gnu/sys/select.h
expr.o: /usr/include/x86_64-linux-gnu/bits/select.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
expr.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
expr.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
expr.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
expr.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
expr.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
expr.o: /usr/include/alloca.h
expr.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
expr.o: /usr/include/stdio.h
expr.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
expr.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
expr.o: /usr/include/linux/string.h /usr/include/linux/errno.h
expr.o: /usr/include/x86_64-linux-gnu/asm/errno.h
expr.o: /usr/include/asm-generic/errno.h
expr.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
expr.o: /usr/include/linux/time.h /usr/include/linux/types.h
expr.o: /usr/include/x86_64-linux-gnu/asm/types.h
expr.o: /usr/include/asm-generic/types.h /usr/include/asm-generic/int-ll64.h
expr.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
expr.o: /usr/include/asm-generic/bitsperlong.h
expr.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
expr.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
expr.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
expr.o: /usr/include/asm-generic/posix_types.h
expr.o: /usr/include/linux/time_types.h /usr/include/math.h
expr.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
expr.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
expr.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
expr.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
expr.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
expr.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
expr.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
expr.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
expr.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
expr.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
expr.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
expr.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
expr.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
expr.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
expr.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
expr.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
expr.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
expr.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
expr.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
expr.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
expr.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
expr.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
expr.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
gcroot.o: unroff.h /usr/include/assert.h /usr/include/features.h
gcroot.o: /usr/include/features-time64.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
gcroot.o: /usr/include/stdc-predef.h
gcroot.o: /usr/include/x86_64-linux-gnu/sys/cdefs.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
gcroot.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
gcroot.o: /usr/include/linux/limits.h /usr/include/stdlib.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
gcroot.o: /usr/include/linux/stddef.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
gcroot.o: /usr/include/x86_64-linux-gnu/sys/types.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/time64.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
gcroot.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
gcroot.o: /usr/include/x86_64-linux-gnu/sys/select.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/select.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
gcroot.o: /usr/include/alloca.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
gcroot.o: /usr/include/stdio.h
gcroot.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
gcroot.o: /usr/include/linux/string.h /usr/include/linux/errno.h
gcroot.o: /usr/include/x86_64-linux-gnu/asm/errno.h
gcroot.o: /usr/include/asm-generic/errno.h
gcroot.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
gcroot.o: /usr/include/linux/time.h /usr/include/linux/types.h
gcroot.o: /usr/include/x86_64-linux-gnu/asm/types.h
gcroot.o: /usr/include/asm-generic/types.h
gcroot.o: /usr/include/asm-generic/int-ll64.h
gcroot.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
gcroot.o: /usr/include/asm-generic/bitsperlong.h
gcroot.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
gcroot.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
gcroot.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
gcroot.o: /usr/include/asm-generic/posix_types.h
gcroot.o: /usr/include/linux/time_types.h /usr/include/math.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
gcroot.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
gcroot.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
gcroot.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
gcroot.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
gcroot.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
gcroot.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
gcroot.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
gcroot.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
gcroot.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
gcroot.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
gcroot.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
gcroot.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
gcroot.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
insert.o: /usr/include/x86_64-linux-gnu/sys/types.h /usr/include/features.h
insert.o: /usr/include/features-time64.h
insert.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
insert.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
insert.o: /usr/include/stdc-predef.h
insert.o: /usr/include/x86_64-linux-gnu/sys/cdefs.h
insert.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
insert.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types.h
insert.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
insert.o: /usr/include/x86_64-linux-gnu/bits/time64.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
insert.o: /usr/include/linux/stddef.h
insert.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
insert.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
insert.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
insert.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
insert.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
insert.o: /usr/include/x86_64-linux-gnu/sys/select.h
insert.o: /usr/include/x86_64-linux-gnu/bits/select.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
insert.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
insert.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
insert.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
insert.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
insert.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
insert.o: /usr/include/x86_64-linux-gnu/sys/stat.h
insert.o: /usr/include/x86_64-linux-gnu/bits/stat.h
insert.o: /usr/include/x86_64-linux-gnu/bits/struct_stat.h
insert.o: /usr/include/linux/fcntl.h
insert.o: /usr/include/x86_64-linux-gnu/asm/fcntl.h
insert.o: /usr/include/asm-generic/fcntl.h /usr/include/linux/types.h
insert.o: /usr/include/x86_64-linux-gnu/asm/types.h
insert.o: /usr/include/asm-generic/types.h
insert.o: /usr/include/asm-generic/int-ll64.h
insert.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
insert.o: /usr/include/asm-generic/bitsperlong.h
insert.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
insert.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
insert.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
insert.o: /usr/include/asm-generic/posix_types.h /usr/include/linux/openat2.h
insert.o: /usr/include/linux/unistd.h
insert.o: /usr/include/x86_64-linux-gnu/asm/unistd.h
insert.o: /usr/include/x86_64-linux-gnu/asm/unistd_64.h unroff.h
insert.o: /usr/include/assert.h /usr/include/linux/limits.h
insert.o: /usr/include/stdlib.h
insert.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
insert.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
insert.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
insert.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
insert.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
insert.o: /usr/include/alloca.h
insert.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
insert.o: /usr/include/stdio.h
insert.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
insert.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
insert.o: /usr/include/linux/string.h /usr/include/linux/errno.h
insert.o: /usr/include/x86_64-linux-gnu/asm/errno.h
insert.o: /usr/include/asm-generic/errno.h
insert.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
insert.o: /usr/include/linux/time.h /usr/include/linux/time_types.h
insert.o: /usr/include/math.h
insert.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
insert.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
insert.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
insert.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
insert.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
insert.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
insert.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
insert.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
insert.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
insert.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
insert.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
insert.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
insert.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
insert.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
insert.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
insert.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
insert.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
insert.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
insert.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
insert.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
insert.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
insert.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
insert.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
malloc.o: unroff.h /usr/include/assert.h /usr/include/features.h
malloc.o: /usr/include/features-time64.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
malloc.o: /usr/include/stdc-predef.h
malloc.o: /usr/include/x86_64-linux-gnu/sys/cdefs.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
malloc.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
malloc.o: /usr/include/linux/limits.h /usr/include/stdlib.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
malloc.o: /usr/include/linux/stddef.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
malloc.o: /usr/include/x86_64-linux-gnu/sys/types.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/time64.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
malloc.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
malloc.o: /usr/include/x86_64-linux-gnu/sys/select.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/select.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
malloc.o: /usr/include/alloca.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
malloc.o: /usr/include/stdio.h
malloc.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
malloc.o: /usr/include/linux/string.h /usr/include/linux/errno.h
malloc.o: /usr/include/x86_64-linux-gnu/asm/errno.h
malloc.o: /usr/include/asm-generic/errno.h
malloc.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
malloc.o: /usr/include/linux/time.h /usr/include/linux/types.h
malloc.o: /usr/include/x86_64-linux-gnu/asm/types.h
malloc.o: /usr/include/asm-generic/types.h
malloc.o: /usr/include/asm-generic/int-ll64.h
malloc.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
malloc.o: /usr/include/asm-generic/bitsperlong.h
malloc.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
malloc.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
malloc.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
malloc.o: /usr/include/asm-generic/posix_types.h
malloc.o: /usr/include/linux/time_types.h /usr/include/math.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
malloc.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
malloc.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
malloc.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
malloc.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
malloc.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
malloc.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
malloc.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
malloc.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
malloc.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
malloc.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
malloc.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
malloc.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
malloc.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
parse.o: unroff.h /usr/include/assert.h /usr/include/features.h
parse.o: /usr/include/features-time64.h
parse.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
parse.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
parse.o: /usr/include/stdc-predef.h /usr/include/x86_64-linux-gnu/sys/cdefs.h
parse.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
parse.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
parse.o: /usr/include/linux/limits.h /usr/include/stdlib.h
parse.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
parse.o: /usr/include/linux/stddef.h
parse.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
parse.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
parse.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
parse.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
parse.o: /usr/include/x86_64-linux-gnu/sys/types.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types.h
parse.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
parse.o: /usr/include/x86_64-linux-gnu/bits/time64.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
parse.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
parse.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
parse.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
parse.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
parse.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
parse.o: /usr/include/x86_64-linux-gnu/sys/select.h
parse.o: /usr/include/x86_64-linux-gnu/bits/select.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
parse.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
parse.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
parse.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
parse.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
parse.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
parse.o: /usr/include/alloca.h
parse.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
parse.o: /usr/include/stdio.h
parse.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
parse.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
parse.o: /usr/include/linux/string.h /usr/include/linux/errno.h
parse.o: /usr/include/x86_64-linux-gnu/asm/errno.h
parse.o: /usr/include/asm-generic/errno.h
parse.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
parse.o: /usr/include/linux/time.h /usr/include/linux/types.h
parse.o: /usr/include/x86_64-linux-gnu/asm/types.h
parse.o: /usr/include/asm-generic/types.h /usr/include/asm-generic/int-ll64.h
parse.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
parse.o: /usr/include/asm-generic/bitsperlong.h
parse.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
parse.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
parse.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
parse.o: /usr/include/asm-generic/posix_types.h
parse.o: /usr/include/linux/time_types.h /usr/include/math.h
parse.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
parse.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
parse.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
parse.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
parse.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
parse.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
parse.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
parse.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
parse.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
parse.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
parse.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
parse.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
parse.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
parse.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
parse.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
parse.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
parse.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
parse.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
parse.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
parse.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
parse.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
parse.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
parse.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
prim.o: unroff.h /usr/include/assert.h /usr/include/features.h
prim.o: /usr/include/features-time64.h
prim.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
prim.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
prim.o: /usr/include/stdc-predef.h /usr/include/x86_64-linux-gnu/sys/cdefs.h
prim.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
prim.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h /usr/include/linux/limits.h
prim.o: /usr/include/stdlib.h
prim.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
prim.o: /usr/include/linux/stddef.h
prim.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
prim.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
prim.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
prim.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
prim.o: /usr/include/x86_64-linux-gnu/sys/types.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types.h
prim.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
prim.o: /usr/include/x86_64-linux-gnu/bits/time64.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
prim.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
prim.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
prim.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
prim.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
prim.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
prim.o: /usr/include/x86_64-linux-gnu/sys/select.h
prim.o: /usr/include/x86_64-linux-gnu/bits/select.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
prim.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
prim.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
prim.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
prim.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
prim.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
prim.o: /usr/include/alloca.h
prim.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
prim.o: /usr/include/stdio.h
prim.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
prim.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
prim.o: /usr/include/linux/string.h /usr/include/linux/errno.h
prim.o: /usr/include/x86_64-linux-gnu/asm/errno.h
prim.o: /usr/include/asm-generic/errno.h
prim.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
prim.o: /usr/include/linux/time.h /usr/include/linux/types.h
prim.o: /usr/include/x86_64-linux-gnu/asm/types.h
prim.o: /usr/include/asm-generic/types.h /usr/include/asm-generic/int-ll64.h
prim.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
prim.o: /usr/include/asm-generic/bitsperlong.h
prim.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
prim.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
prim.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
prim.o: /usr/include/asm-generic/posix_types.h
prim.o: /usr/include/linux/time_types.h /usr/include/math.h
prim.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
prim.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
prim.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
prim.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
prim.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
prim.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
prim.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
prim.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
prim.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
prim.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
prim.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
prim.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
prim.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
prim.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
prim.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
prim.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
prim.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
prim.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
prim.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
prim.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
prim.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
prim.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
prim.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
scmtable.o: unroff.h /usr/include/assert.h /usr/include/features.h
scmtable.o: /usr/include/features-time64.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
scmtable.o: /usr/include/stdc-predef.h
scmtable.o: /usr/include/x86_64-linux-gnu/sys/cdefs.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
scmtable.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
scmtable.o: /usr/include/linux/limits.h /usr/include/stdlib.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
scmtable.o: /usr/include/linux/stddef.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
scmtable.o: /usr/include/x86_64-linux-gnu/sys/types.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/time64.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
scmtable.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
scmtable.o: /usr/include/x86_64-linux-gnu/sys/select.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/select.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
scmtable.o: /usr/include/alloca.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
scmtable.o: /usr/include/stdio.h
scmtable.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
scmtable.o: /usr/include/linux/string.h /usr/include/linux/errno.h
scmtable.o: /usr/include/x86_64-linux-gnu/asm/errno.h
scmtable.o: /usr/include/asm-generic/errno.h
scmtable.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
scmtable.o: /usr/include/linux/time.h /usr/include/linux/types.h
scmtable.o: /usr/include/x86_64-linux-gnu/asm/types.h
scmtable.o: /usr/include/asm-generic/types.h
scmtable.o: /usr/include/asm-generic/int-ll64.h
scmtable.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
scmtable.o: /usr/include/asm-generic/bitsperlong.h
scmtable.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
scmtable.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
scmtable.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
scmtable.o: /usr/include/asm-generic/posix_types.h
scmtable.o: /usr/include/linux/time_types.h /usr/include/math.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
scmtable.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
scmtable.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
scmtable.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
scmtable.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
scmtable.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
scmtable.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
scmtable.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
scmtable.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
scmtable.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
scmtable.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
scmtable.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
scmtable.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
scmtable.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
stream.o: unroff.h /usr/include/assert.h /usr/include/features.h
stream.o: /usr/include/features-time64.h
stream.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
stream.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
stream.o: /usr/include/stdc-predef.h
stream.o: /usr/include/x86_64-linux-gnu/sys/cdefs.h
stream.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
stream.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
stream.o: /usr/include/linux/limits.h /usr/include/stdlib.h
stream.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
stream.o: /usr/include/linux/stddef.h
stream.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
stream.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
stream.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
stream.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
stream.o: /usr/include/x86_64-linux-gnu/sys/types.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types.h
stream.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
stream.o: /usr/include/x86_64-linux-gnu/bits/time64.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
stream.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
stream.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
stream.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
stream.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
stream.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
stream.o: /usr/include/x86_64-linux-gnu/sys/select.h
stream.o: /usr/include/x86_64-linux-gnu/bits/select.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
stream.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
stream.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
stream.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
stream.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
stream.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
stream.o: /usr/include/alloca.h
stream.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
stream.o: /usr/include/stdio.h
stream.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
stream.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
stream.o: /usr/include/linux/string.h /usr/include/linux/errno.h
stream.o: /usr/include/x86_64-linux-gnu/asm/errno.h
stream.o: /usr/include/asm-generic/errno.h
stream.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
stream.o: /usr/include/linux/time.h /usr/include/linux/types.h
stream.o: /usr/include/x86_64-linux-gnu/asm/types.h
stream.o: /usr/include/asm-generic/types.h
stream.o: /usr/include/asm-generic/int-ll64.h
stream.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
stream.o: /usr/include/asm-generic/bitsperlong.h
stream.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
stream.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
stream.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
stream.o: /usr/include/asm-generic/posix_types.h
stream.o: /usr/include/linux/time_types.h /usr/include/math.h
stream.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
stream.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
stream.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
stream.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
stream.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
stream.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
stream.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
stream.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
stream.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
stream.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
stream.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
stream.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
stream.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
stream.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
stream.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
stream.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
stream.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
stream.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
stream.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
stream.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
stream.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
stream.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
stream.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
subst.o: unroff.h /usr/include/assert.h /usr/include/features.h
subst.o: /usr/include/features-time64.h
subst.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
subst.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
subst.o: /usr/include/stdc-predef.h /usr/include/x86_64-linux-gnu/sys/cdefs.h
subst.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
subst.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
subst.o: /usr/include/linux/limits.h /usr/include/stdlib.h
subst.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
subst.o: /usr/include/linux/stddef.h
subst.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
subst.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
subst.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
subst.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
subst.o: /usr/include/x86_64-linux-gnu/sys/types.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types.h
subst.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
subst.o: /usr/include/x86_64-linux-gnu/bits/time64.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
subst.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
subst.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
subst.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
subst.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
subst.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
subst.o: /usr/include/x86_64-linux-gnu/sys/select.h
subst.o: /usr/include/x86_64-linux-gnu/bits/select.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
subst.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
subst.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
subst.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
subst.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
subst.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
subst.o: /usr/include/alloca.h
subst.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
subst.o: /usr/include/stdio.h
subst.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
subst.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
subst.o: /usr/include/linux/string.h /usr/include/linux/errno.h
subst.o: /usr/include/x86_64-linux-gnu/asm/errno.h
subst.o: /usr/include/asm-generic/errno.h
subst.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
subst.o: /usr/include/linux/time.h /usr/include/linux/types.h
subst.o: /usr/include/x86_64-linux-gnu/asm/types.h
subst.o: /usr/include/asm-generic/types.h /usr/include/asm-generic/int-ll64.h
subst.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
subst.o: /usr/include/asm-generic/bitsperlong.h
subst.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
subst.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
subst.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
subst.o: /usr/include/asm-generic/posix_types.h
subst.o: /usr/include/linux/time_types.h /usr/include/math.h
subst.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
subst.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
subst.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
subst.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
subst.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
subst.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
subst.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
subst.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
subst.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
subst.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
subst.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
subst.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
subst.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
subst.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
subst.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
subst.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
subst.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
subst.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
subst.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
subst.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
subst.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
subst.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
subst.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
table.o: unroff.h /usr/include/assert.h /usr/include/features.h
table.o: /usr/include/features-time64.h
table.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
table.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
table.o: /usr/include/stdc-predef.h /usr/include/x86_64-linux-gnu/sys/cdefs.h
table.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
table.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
table.o: /usr/include/linux/limits.h /usr/include/stdlib.h
table.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
table.o: /usr/include/linux/stddef.h
table.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
table.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
table.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
table.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
table.o: /usr/include/x86_64-linux-gnu/sys/types.h
table.o: /usr/include/x86_64-linux-gnu/bits/types.h
table.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
table.o: /usr/include/x86_64-linux-gnu/bits/time64.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
table.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
table.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
table.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
table.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
table.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
table.o: /usr/include/x86_64-linux-gnu/sys/select.h
table.o: /usr/include/x86_64-linux-gnu/bits/select.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
table.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
table.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
table.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
table.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
table.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
table.o: /usr/include/alloca.h
table.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
table.o: /usr/include/stdio.h
table.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
table.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
table.o: /usr/include/linux/string.h /usr/include/linux/errno.h
table.o: /usr/include/x86_64-linux-gnu/asm/errno.h
table.o: /usr/include/asm-generic/errno.h
table.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
table.o: /usr/include/linux/time.h /usr/include/linux/types.h
table.o: /usr/include/x86_64-linux-gnu/asm/types.h
table.o: /usr/include/asm-generic/types.h /usr/include/asm-generic/int-ll64.h
table.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
table.o: /usr/include/asm-generic/bitsperlong.h
table.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
table.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
table.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
table.o: /usr/include/asm-generic/posix_types.h
table.o: /usr/include/linux/time_types.h /usr/include/math.h
table.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
table.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
table.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
table.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
table.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
table.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
table.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
table.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
table.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
table.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
table.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
table.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
table.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
table.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
table.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
table.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
table.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
table.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
table.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
table.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
table.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
table.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
table.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h
unroff.o: unroff.h /usr/include/assert.h /usr/include/features.h
unroff.o: /usr/include/features-time64.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/wordsize.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/timesize.h
unroff.o: /usr/include/stdc-predef.h
unroff.o: /usr/include/x86_64-linux-gnu/sys/cdefs.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/long-double.h
unroff.o: /usr/include/x86_64-linux-gnu/gnu/stubs.h
unroff.o: /usr/include/linux/limits.h /usr/include/stdlib.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
unroff.o: /usr/include/linux/stddef.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/waitflags.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/waitstatus.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/floatn.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/floatn-common.h
unroff.o: /usr/include/x86_64-linux-gnu/sys/types.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/typesizes.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/time64.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/time_t.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
unroff.o: /usr/include/endian.h /usr/include/x86_64-linux-gnu/bits/endian.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/endianness.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/byteswap.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
unroff.o: /usr/include/x86_64-linux-gnu/sys/select.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/select.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
unroff.o: /usr/include/alloca.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
unroff.o: /usr/include/stdio.h
unroff.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdarg.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/FILE.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
unroff.o: /usr/include/linux/string.h /usr/include/linux/errno.h
unroff.o: /usr/include/x86_64-linux-gnu/asm/errno.h
unroff.o: /usr/include/asm-generic/errno.h
unroff.o: /usr/include/asm-generic/errno-base.h /usr/include/ctype.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
unroff.o: /usr/include/linux/time.h /usr/include/linux/types.h
unroff.o: /usr/include/x86_64-linux-gnu/asm/types.h
unroff.o: /usr/include/asm-generic/types.h
unroff.o: /usr/include/asm-generic/int-ll64.h
unroff.o: /usr/include/x86_64-linux-gnu/asm/bitsperlong.h
unroff.o: /usr/include/asm-generic/bitsperlong.h
unroff.o: /usr/include/linux/posix_types.h /usr/include/linux/stddef.h
unroff.o: /usr/include/x86_64-linux-gnu/asm/posix_types.h
unroff.o: /usr/include/x86_64-linux-gnu/asm/posix_types_64.h
unroff.o: /usr/include/asm-generic/posix_types.h
unroff.o: /usr/include/linux/time_types.h /usr/include/math.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/math-vector.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/fp-logb.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/fp-fast.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/mathcalls.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/mathcalls-narrow.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/iscanonical.h
unroff.o: /usr/include/memory.h /usr/local/include/elk/scheme.h
unroff.o: /usr/local/include/elk/site.h /usr/include/inttypes.h
unroff.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint.h
unroff.o: /usr/lib/gcc/x86_64-linux-gnu/11/include/stdint-gcc.h
unroff.o: /usr/local/include/elk/funcproto.h /usr/local/include/elk/param.h
unroff.o: /usr/local/include/elk/object.h /usr/include/setjmp.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/setjmp.h
unroff.o: /usr/include/x86_64-linux-gnu/bits/types/struct___jmp_buf_tag.h
unroff.o: /usr/local/include/elk/extern.h /usr/local/include/elk/misc.h
unroff.o: /usr/local/include/elk/exception.h /usr/local/include/elk/gc.h
unroff.o: /usr/local/include/elk/type.h /usr/local/include/elk/stkmem.h
unroff.o: /usr/local/include/elk/cstring.h /usr/local/include/elk/compat.h
unroff.o: config.h args.h buffer.h table.h error.h event.h expr.h gcroot.h
unroff.o: insert.h malloc.h parse.h prim.h scmtable.h stream.h subst.h

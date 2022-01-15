/* $Revision: 1.12 $
 */

#include "unroff.h"

static char *progname = "(whoami?)";

void set_progname(char *s) {
    char *p;

    progname = (p = strrchr(s, '/')) ? p+1 : s;
}

char *get_progname(void) {
    return progname;
}

void warn(char *fmt, ...) {
    va_list args;

    fprintf(stderr, "%s: ", progname);
    if (istream_is_open())
	fprintf(stderr, "%s:%lu: ", curr_istream_target(), curr_istream_lno());
    fprintf(stderr, "warning: ");
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ".\n");
}

void fatal_error(char *fmt, ...) {
    va_list args;

    fprintf(stderr, "%s: ", progname);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ".\n");
    exit(1);
}

static char *strerr(void) {
    extern int sys_nerr;
    extern char *sys_errlist[];

    return errno > 0 && errno < sys_nerr ?
	sys_errlist[errno] : "unknown error";
}

void read_error(char *s) {
    fatal_error("reading %s: %s", s, strerr());
}

void write_error(char *s) {
    fatal_error("writing %s: %s", s, strerr());
}

void open_error(char *fn) {
    fatal_error("%s: %s", fn, strerr());
}

char *printable_string(char *s, int len) {
    static Buffer *bp;
    char c[4];

    if (!bp)
	bp = buffer_new(0);
    buffer_clear(bp);
    for ( ; len > 0; len--, s++) {
	if (isprint(UCHAR(*s)) || *s == ' ') {
	    buffer_putc(bp, *s);
	} else {
	    buffer_putc(bp, '\\');
	    switch (*s) {
	    case '\n':
		buffer_putc(bp, 'n'); break;
	    case '\t':
		buffer_putc(bp, 't'); break;
	    case '\b':
		buffer_putc(bp, 'b'); break;
	    default:
		sprintf(c, "%03o", *s);
		buffer_puts(bp, c, 3);
	    }
	}
    }
    buffer_putc(bp, '\0');
    return bp->data;
}

char *printable_char(char c) {
    return printable_string(&c, 1);
}

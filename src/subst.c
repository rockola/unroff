/* $Revision: 1.9 $
 */

/* The implementation of the Scheme primitive `substitute'
 */

#include "unroff.h"

Object p_substitute(int ac, Object *av) {
    Object str = *av;
    static Buffer *fp;
    char *p, *ep, *s, *t, *endp;
    int len;
    long a;
    char buf[30];
    char *fmt;
    time_t now;
    extern char *macros, *format, *directory;

    Check_Type(str, T_String);
    if (!fp)
	fp = buffer_new(0);
    buffer_clear(fp);
    for (p = STRING(str)->data, ep = p + STRING(str)->size; p < ep; p++) {
	if (*p == '%') {
	    for (s = ++p; p < ep && *p != '%'; p++)
		;
	    if (p == ep)
		Primitive_Error("missing `%' delimiter");
	    len = p-s;
	    t = safe_malloc(len+1);   /* Make copy to add \0.  C sucks... */
	    memcpy(t, s, len);
	    t[len] = 0;
	    fmt = 0;
	    if (len == 0)
		s = "%";
	    else if (strcmp(t, "macros") == 0)
		s = macros;
	    else if (strcmp(t, "format") == 0)
		s = format;
	    else if (strcmp(t, "directory") == 0)
		s = directory;
	    else if (strcmp(t, "progname") == 0)
		s = get_progname();
	    else if (strcmp(t, "filepos") == 0) {    /* ` file:lno' */
		if (istream_is_open()) {
		    buffer_putc(fp, ' ');
		    buffer_puts(fp, curr_istream_target(),
			strlen(curr_istream_target()));
		    sprintf(buf, ":%lu:", curr_istream_lno());
		    s = buf;
		} else s = "";
	    } else if (strcmp(t, "tmpname") == 0) {
		s = tmpnam(0);
	    } else if (strcmp(t, "version") == 0) {
		sprintf(buf, "%d.%d", MAJOR_VERSION, MINOR_VERSION);
		s = buf;
	    } else if (strcmp(t, "weekday") == 0) {
		fmt = "%a";
	    } else if (strcmp(t, "weekday+") == 0) {
		fmt = "%A";
	    } else if (strcmp(t, "weekdaynum")== 0) {
		fmt = "%w";
	    } else if (strcmp(t, "monthname") == 0) {
		fmt = "%b";
	    } else if (strcmp(t, "monthname+") == 0) {
		fmt = "%B";
	    } else if (strcmp(t, "day") == 0) {
		fmt = "%d";
	    } else if (strcmp(t, "month") == 0) {
		fmt = "%m";
	    } else if (strcmp(t, "year") == 0) {
		fmt = "%Y";
	    } else if (strcmp(t, "date") == 0) {
		fmt = "%x";
	    } else if (strcmp(t, "time") == 0) {
		fmt = "%X";
	    } else if ((a = strtol(t, &endp, 10)), endp == t+len) {
		if (a < 1 || a > ac-1)
		    Primitive_Error("no argument for %-specifier");
		Check_Type(av[a], T_String);
		buffer_puts(fp, STRING(av[a])->data, STRING(av[a])->size);
		s = "";
	    } else if ((s = getenv(t)) == 0)
		s = "";
	    if (fmt) {
		now = time(0);
		if (strftime(buf, sizeof buf, fmt, localtime(&now)) == 0)
		    s = "";
		else s = buf;
	    }
	    free(t);
	    buffer_puts(fp, s, strlen(s));
	} else
	    buffer_putc(fp, *p);
    }
    return Make_String(fp->data, fp->size);
}

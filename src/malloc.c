/* $Revision: 1.1 $
 */

#include "unroff.h"

static void (*err_handler)(char *, size_t);
static void default_err_handler(char *, size_t);

void *safe_malloc(size_t n) {
    void *p;

    if ((p = malloc(n)) == 0) {
	(err_handler ? err_handler : default_err_handler)("malloc", n);
	exit(1);
    }
    return p;
}
	
void *safe_realloc(void *old, size_t n) {
    void *p;

    if ((p = realloc(old, n)) == 0) {
	(err_handler ? err_handler : default_err_handler)("realloc", n);
	exit(1);
    }
    return p;
}

void set_alloc_failure(void (*func)(char *, size_t)) {
    err_handler = func;
}

static void default_err_handler(char *what, size_t n) {
    fatal_error("cannot %s %lu bytes--virtual memory exhausted",
	what, (unsigned long)n);
}


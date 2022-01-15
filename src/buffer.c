/* $Revision: 1.7 $
 */

/* `Buffer' is a dynamically growing, general-purpose buffer data
 * structure.
 *
 * buffer_new(size)         --  returns new buffer with initial size `size'
 *                              or some default size if argument is zero
 * buffer_delete(b)         --  marks a buffer as unused
 * buffer_clear(b)          --  empties a buffer
 * buffer_puts(b,data,len)  --  appends data to a buffer, growing it if
 *                              necessary
 * buffer_putc(b,c)         --  appends a character to a buffer
 */

#include "unroff.h"

static int sizeinc_linear(Buffer *);

static Buffer *first_free;

Buffer *buffer_new(int size) {
    Buffer *p;

    if (size == 0)
	size = 512;
    if (first_free) {    /* reuse a buffer if there is one */
	p = first_free;
	first_free = p->next;
	if (p->max < size) {
	    free(p->data);
	    p->data = safe_malloc(p->max = size);
	}
    } else {
	p = safe_malloc(sizeof *p);
	p->data = safe_malloc(size);
	p->increment = p->max = size;
	p->sizeinc_func = sizeinc_linear;
    }
    buffer_clear(p);
    return p;
}

void buffer_delete(Buffer *p) {
    assert(p->size >= 0);
    p->size = -1;
    p->next = first_free;
    first_free = p;
}

void buffer_grow(Buffer *p) {
    p->max = p->sizeinc_func(p);
    p->data = safe_realloc(p->data, p->max);
}

void buffer_puts(Buffer *p, char *s, int size) {
    assert(size >= 0);
    while (p->size + size > p->max)
	buffer_grow(p);
    if (size == 0)
	return;
    memcpy(p->data+p->size, s, size);
    p->size += size;
}

static int sizeinc_linear(Buffer *p) {
    return p->max + p->increment;
}

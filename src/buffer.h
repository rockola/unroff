/* $Revision: 1.4 $
 */

typedef struct _buffer {
    char *data;
    int increment;
    int size;
    int max;
    int (*sizeinc_func)(struct _buffer *);
    struct _buffer *next;
} Buffer;

Buffer *buffer_new(int);
void buffer_delete(Buffer *);
void buffer_grow(Buffer *);
void buffer_puts(Buffer *, char *, int);

#define buffer_need_grow(p)     ((p)->size == (p)->max)
#define buffer_clear(p)         ((p)->size = 0)

#define buffer_putc(p, c) {\
    if (buffer_need_grow(p))\
	buffer_grow(p);\
    (p)->data[(p)->size++] = (c);\
}


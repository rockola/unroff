/* $Revision: 1.14 $
 */

/* The implementation of the Scheme type `stream' and the primitives
 * that work on streams.  Additional functions exported by this module:
 * 
 * istream_is_open()     --  true if current input stream != #f
 * ostream_is_open()     --  true if current output stream != #f
 * curr_istream_target() --  returns target of current input stream
 * curr_istream_lno()    --  returns current istream's input line number
 * safe_readline(buffer) --  reads line from current istream into buffer;
 *                           signals error if istream is not open
 * safe_write_char(c)    --  sends character to current ostream or to
 *                           stdout if ostream is #f
 * safe_write(data,len)  --  same, but writes several characters
 */


#include "unroff.h"

typedef struct _stream {
    Object tag;
    char open;
    char type;
    char direction;
    char *target;
    FILE *fp;
    Buffer *bp;
    int bs;
    unsigned long lno;
    unsigned long pos;
    Buffer *unread;
    int (*readline)(struct _stream *, Buffer *);
    void (*write)(struct _stream *, char *, int);
    void (*close)(struct _stream *);
} Stream;

static Object buffers;
static Object istream, ostream;

#define STREAM(x)    ((Stream *)POINTER(x))

static int T_Stream;

static Object p_streamp(Object x) {
    return TYPE(x) == T_Stream ? True : False;
}

static int stream_equal(Object s1, Object s2) {
    return EQ(s1, s2);
}

static int stream_print(Object x, Object port, int raw, int depth,
	int length) {
    Stream *p = STREAM(x);

    if (p->open || p->type == 'b')
	Printf(port, "#[stream %s]", p->target);
    else
	Printf(port, "#[stream %lu]", POINTER(x));
    return 0;
}

static Object terminate_stream(Object x) {
    Stream *p = STREAM(x);

    if (p->open && !(p->type == 'b' && p->direction == 'o')) {
	free(p->target);
	buffer_delete(p->unread);
	if (p->type != 'b')
	    p->close(p);
    }
    p->open = 0;
    return Void;
}

int istream_is_open(void) {
    return Truep(istream);
}

char *curr_istream_target(void) {
    assert(Truep(istream));
    assert(STREAM(istream)->open);
    return STREAM(istream)->target;
}

unsigned long curr_istream_lno(void) {
    return STREAM(istream)->lno;
}

static int stream_is_active(Object str) {
    return EQ(istream, str) || EQ(ostream, str);
}

int safe_readline(Buffer *bp) {
    Stream *str;

    if (!Truep(istream))
	Primitive_Error("no input stream defined");
    str = STREAM(istream);
    if (str->unread->size > 0) {
	buffer_puts(bp, str->unread->data, str->unread->size);
	if (bp->data[bp->size-1] != '\n')
	    buffer_putc(bp, '\n');
	buffer_clear(str->unread);
	return 0;
    }
    return str->readline(str, bp);
}

void safe_write_char(char c) {
    if (Truep(ostream)) {
	STREAM(ostream)->pos++;
	STREAM(ostream)->write(STREAM(ostream), &c, 1);
    } else if (putc(c, stdout) == EOF)
	write_error("stdout");
}

void safe_write(char *data, int len) {
    if (len == 0)
	return;
    if (Truep(ostream)) {
	STREAM(ostream)->pos += len;
	STREAM(ostream)->write(STREAM(ostream), data, len);
    } else if (fwrite(data, len, 1, stdout) == 0)
	write_error("stdout");
}
	
#define is_continuation(p) \
    ((p)->size > oldsize && (p)->data[(p)->size-1] == escape &&\
	!((p)->size > oldsize+1 && (p)->data[(p)->size-2] == escape))

static int readline_buffer(Stream *self, Buffer *bp) {
    int oldsize;
    int c;
    Buffer *sp = self->bp;

    assert(self->bs <= sp->size);
    if (self->bs == sp->size)
	return 1;
    oldsize = bp->size;
    while (self->bs < sp->size) {
	if ((c = sp->data[self->bs++]) == '\n') {
	    self->lno++;
	    if (is_continuation(bp)) {
		bp->size--;
	    } else {
		buffer_putc(bp, c);
		return 0;
	    }
	} else buffer_putc(bp, c);
    }
    if (bp->size > oldsize) {
	buffer_putc(bp, '\n');
	self->lno++;
    }
    return 1;
}

static int readline_file(Stream *self, Buffer *bp) {
    int oldsize;
    int c;

    if (feof(self->fp))
	return 1;
    oldsize = bp->size;
    while ((c = getc(self->fp)) != EOF) {
	if (c == '\n') {
	    self->lno++;
	    if (is_continuation(bp)) {
		bp->size--;
	    } else {
		buffer_putc(bp, c);
		return 0;
	    }
	} else buffer_putc(bp, c);
    }
    if (ferror(self->fp))
	read_error(self->target);
    if (bp->size > oldsize) {
	buffer_putc(bp, '\n');
	self->lno++;
    }
    return 1;
}

static void write_buffer(Stream *self, char *data, int len) {
    buffer_puts(self->bp, data, len);
}

static void write_file(Stream *self, char *data, int len) {
    if (fwrite(data, len, 1, self->fp) == 0)
	write_error(self->target);
}

static void close_file(Stream *self) {
    (void)fclose(self->fp);
}

static void close_pipe(Stream *self) {
    (void)pclose(self->fp);
}

static Object find_buffer(char *s) {
    Object p;

    for (p = buffers; !Nullp(p); p = Cdr(p)) {
	if (strcmp(STREAM(Car(p))->target, s) == 0)
	    return Car(p);
    }
    return Null;
}

static int target_is_buffer(char *s) {
    int len = strlen(s);

    return len > 1 && s[0] == '[' && s[len-1] == ']';
}

static Object open_stream(Object target, char direction, int append) {
    char *t = Get_Strsym(target), *mode;
    Stream *p;
    Object ret = Null, b = Null;
    GC_Node3;

    GC_Link3(target, ret, b);
    if (target_is_buffer(t)) {
	b = find_buffer(t);
	if (!Nullp(b)) {
	    p = STREAM(b);
	    assert(p->type == 'b');
	    assert(p->direction == 'o');
	    if (p->open)
		Primitive_Error("stream ~s is already open", b);
	    if (direction == 'o') {
		p->open = 1;
		p->lno = p->bs = 0;
		if (!append) {
		    p->pos = 0;
		    buffer_clear(p->bp);
		}
		GC_Unlink;
		return b;
	    }
	}
    }
    ret = Alloc_Object(sizeof(Stream), T_Stream, 0);
    p = STREAM(ret);
    p->tag = Null;
    p->open = 1;
    p->direction = direction;
    p->lno = p->pos = 0;
    p->unread = buffer_new(0);
    p->target = safe_malloc(strlen(t) + 1);
    strcpy(p->target, t);
    if (target_is_buffer(t)) {
	p->readline = readline_buffer;
	p->write = write_buffer;       /* no close function */
	p->type = 'b';
	p->bp = buffer_new(0);
	if (direction == 'o') {
	    buffers = Cons(ret, buffers);
	} else {
	    p->bs = 0;
	    if (!Nullp(b))
		buffer_puts(p->bp, STREAM(b)->bp->data, STREAM(b)->bp->size);
	}
    } else {
	mode = direction == 'i' ? "r" : append ? "a" : "w";
	p->readline = readline_file;
	p->write = write_file;
	if (t[0] == '|') {
	    char *s;
	    if ((p->fp = popen(t+1, mode)) == 0)
		Primitive_Error("cannot open pipe to ~s", target);
	    if ((s = strchr(p->target, ' ')) != 0)
		*s = 0;
	    p->close = close_pipe;
	    p->type = 'p';
	} else {
	    if (direction == 'i' && strcmp(t, "stdin") == 0) {
		p->fp = stdin;
	    } else if ((p->fp = fopen(t, mode)) == 0) {
		Saved_Errno = errno;
		Primitive_Error("cannot open ~s: ~E", target);
	    }
	    p->close = close_file;
	    p->type = 'f';
	}
    }
    Register_Object(ret, (GENERIC)0, terminate_stream, 0);
    GC_Unlink;
    return ret;
}

Object p_open_input_stream(Object target) {
    return open_stream(target, 'i', 0);
}

static Object p_open_output_stream(Object target) {
    return open_stream(target, 'o', 0);
}

static Object p_append_output_stream(Object target) {
    return open_stream(target, 'o', 1);
}

Object p_close_stream(Object x) {
    if (!Truep(x))
	return Void;
    Check_Type(x, T_Stream);
    if (!STREAM(x)->open)
	return Void;
    if (stream_is_active(x))
	Primitive_Error("stream ~s is still in use", x);
    return terminate_stream(x);
}

static Object set_stream(Object x, Object *which) {
    Object ret = *which;
    Stream *p;

    if (Truep(*which) && STREAM(*which)->type != 'b' &&
	    STREAM(*which)->direction == 'o')
	(void)fflush(STREAM(*which)->fp);
    if (Truep(x)) {
	Check_Type(x, T_Stream);
	p = STREAM(x);
	if (!p->open)
	    Primitive_Error("stream ~s has been closed", x);
	if (stream_is_active(x))
	    Primitive_Error("stream ~s is already in use", x);
	if (which == &istream && p->direction != 'i')
	    Primitive_Error("stream ~s is not an input stream", x);
	if (which == &ostream && p->direction != 'o')
	    Primitive_Error("stream ~s is not an output stream", x);
	*which = x;
    } else {
	*which = False;
    }
    return ret;
}

Object p_set_input_stream(Object x) {
    return set_stream(x, &istream);
}

static Object p_set_output_stream(Object x) {
    return set_stream(x, &ostream);
}

static Object p_input_stream(void) {
    return istream;
}

static Object p_output_stream(void) {
    return ostream;
}

static Object p_unread_line(Object str) {
    Check_Type(str, T_String);
    if (!Truep(istream))
	Primitive_Error("no input stream defined");
    buffer_puts(STREAM(istream)->unread, STRING(str)->data,
	STRING(str)->size);
    return Void;
}

#define stream_type_pred(what,t)\
    static Object p_stream_##what(Object x) {\
	if (Truep(x)) {\
	    Check_Type(x, T_Stream);\
	    return STREAM(x)->type == t ? True : False;\
	} else return False;\
}
stream_type_pred(buffer, 'b')
stream_type_pred(file, 'f')
stream_type_pred(pipe, 'p')

static Object p_stream_target(Object x) {
    if (!Truep(x))
	return Make_String("", 0);
    Check_Type(x, T_Stream);
    return Make_String(STREAM(x)->target, strlen(STREAM(x)->target));
}

static Object p_stream_to_string(Object target) {
    Object str, old, ret;
    Stream *sp;
    Buffer *bp;

    str = p_open_input_stream(target);
    old = p_set_input_stream(str);
    bp = buffer_new(0);
    for (sp = STREAM(str); sp->readline(sp, bp) == 0; )
	;
    (void)p_set_input_stream(old);
    (void)p_close_stream(str);
    ret = Make_String(bp->data, bp->size);
    buffer_delete(bp);
    return ret;
}

static Object p_stream_position(Object x) {
    if (!Truep(x))
	return Make_Integer(0);
    Check_Type(x, T_Stream);
    return Make_Unsigned_Long(STREAM(x)->pos);
}

void init_stream(void) {
    istream = ostream = False;
    buffers = Null;
    Global_GC_Link(istream);
    Global_GC_Link(ostream);
    Global_GC_Link(buffers);
    T_Stream = Define_Type(0, "stream", NOFUNC, sizeof(Stream),
	stream_equal, stream_equal, stream_print, NOFUNC);
    Define_Primitive(p_streamp,         "stream?", 1, 1, EVAL);
    Define_Primitive(p_open_input_stream,
					"open-input-stream", 1, 1, EVAL);
    Define_Primitive(p_open_output_stream,
					"open-output-stream", 1, 1, EVAL);
    Define_Primitive(p_append_output_stream,
					"append-output-stream", 1, 1, EVAL);
    Define_Primitive(p_close_stream,    "close-stream", 1, 1, EVAL);
    Define_Primitive(p_set_input_stream,
					"set-input-stream!", 1, 1, EVAL);
    Define_Primitive(p_set_output_stream,
					"set-output-stream!", 1, 1, EVAL);
    Define_Primitive(p_input_stream,    "input-stream", 0, 0, EVAL);
    Define_Primitive(p_output_stream,   "output-stream", 0, 0, EVAL);
    Define_Primitive(p_unread_line,     "unread-line", 1, 1, EVAL);
    Define_Primitive(p_stream_buffer,   "stream-buffer?", 1, 1, EVAL);
    Define_Primitive(p_stream_file,     "stream-file?", 1, 1, EVAL);
    Define_Primitive(p_stream_pipe,     "stream-pipe?", 1, 1, EVAL);
    Define_Primitive(p_stream_target,   "stream-target", 1, 1, EVAL);
    Define_Primitive(p_stream_to_string,"stream->string", 1, 1, EVAL);
    Define_Primitive(p_stream_position, "stream-position", 1, 1, EVAL);
}

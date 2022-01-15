/* $Revision: 1.19 $
 */

/* The implementation of those Scheme primitives that do not deal with
 * events, tables, streams, expressions, substitutions, and file
 * insertions.
 */

#include "unroff.h"

static Object error_port;

static Object p_error_port(void) {
    return error_port;
}

static Object p_read_line_expand(void) {
    Buffer *ip, *op;
    Object ret;

    ip = buffer_new(0);
    op = buffer_new(0);
    if (safe_readline(ip) && ip->size == 0) {
	ret = Eof;
    } else {
	(void)parse_expand(ip, op);
	ret = Make_String(op->data, op->size);
    }
    buffer_delete(ip);
    buffer_delete(op);
    return ret;
}

static Object p_read_line(void) {
    Buffer *ip;
    Object ret;

    ip = buffer_new(0);
    if (safe_readline(ip) && ip->size == 0) {
	ret = Eof;
    } else
	ret = Make_String(ip->data, ip->size);
    buffer_delete(ip);
    return ret;
}

static Object primitive_parse(int ac, Object *av, int what) {
    Buffer *ip, *op;
    Object ret;

    ip = buffer_new(0);
    op = buffer_new(0);
    while (ac-- > 0) {
	Object x = *av++;
	switch (TYPE(x)) {
	case T_Character:
	    buffer_putc(ip, CHAR(x));
	    break;
	case T_Symbol:
	    x = SYMBOL(x)->name;    /* fall through */
	case T_String:
	    buffer_puts(ip, STRING(x)->data, STRING(x)->size);
	    break;
	default:
	    Primitive_Error("cannot coerce argument to string");
	}
    }
    switch (what) {
    case 'c': parse_escape(ip, op, 1, 1); break;
    case 'p': parse_escape(ip, op, 1, 0); break;
    case 't': parse_escape(ip, op, 0, 0); break;
    case 'l': parse_line(ip, op); break;
    case 'e': parse_expand(ip, op); break;
    }
    ret = what == 'l' ? Void : Make_String(op->data, op->size);
    buffer_delete(ip);
    buffer_delete(op);
    return ret;
}

static Object p_parse(int ac, Object *av) {
    return primitive_parse(ac, av, 'p');
}

static Object p_translate(int ac, Object *av) {
    return primitive_parse(ac, av, 't');
}

static Object p_parse_line(int ac, Object *av) {
    return primitive_parse(ac, av, 'l');
}

static Object p_parse_copy_mode(int ac, Object *av) {
    return primitive_parse(ac, av, 'c');
}

static Object p_parse_expand(int ac, Object *av) {
    return primitive_parse(ac, av, 'e');
}

static Object concat(int ac, Object *av, int spread) {
    Buffer *op;
    Object ret;

    op = buffer_new(0);
    while (ac-- > 0) {
	Object x = *av++;
	switch (TYPE(x)) {
	case T_Character:
	    buffer_putc(op, CHAR(x));
	    break;
	case T_Symbol:
	    x = SYMBOL(x)->name;    /* fall through */
	case T_String:
	    buffer_puts(op, STRING(x)->data, STRING(x)->size);
	    break;
	default:
	    Primitive_Error("cannot coerce argument to string");
	}
	if (spread && ac > 0)
	    buffer_putc(op, ' ')
    }
    ret = Make_String(op->data, op->size);
    buffer_delete(op);
    return ret;
}

static Object p_concat(int ac, Object *av) {
    return concat(ac, av, 0);
}

static Object p_spread(int ac, Object *av) {
    return concat(ac, av, 1);
}

static Object p_emit(int ac, Object *av) {
    while (ac-- > 0) {
	Object x = *av++;
	switch (TYPE(x)) {
	case T_Character:
	    safe_write_char(CHAR(x));
	    break;
	case T_Symbol:
	    x = SYMBOL(x)->name;    /* fall through */
	case T_String:
	    safe_write(STRING(x)->data, STRING(x)->size);
	    break;
	default:
	    Primitive_Error("cannot coerce argument to string");
	}
    }
    return Void;
}

static Object p_shell_command(Object cmd) {
    return Make_Integer(system(Get_Strsym(cmd)));
}

static Object p_remove_file(Object fn) {
    char *s = Get_Strsym(fn);

    if (remove(s)) warn("cannot remove file `%s'", s);
    return Void;
}

static Object p_parse_pair(Object x) {
    char *p, *ep, *s1, *s2, delim;
    Object str, ret;
    GC_Node3;

    str = ret = False;
    GC_Link3(x, str, ret);
    Check_Type(x, T_String);
    p = STRING(x)->data;
    ep = p + STRING(x)->size;
    if (p <= ep-3) {
	delim = *p++;
	for (s1 = p; p < ep && *p != delim; p++)
	    ;
	if (p < ep) {
	    for (s2 = ++p; p < ep && *p != delim; p++)
		;
	    if (p == ep-1) {
		str = Make_String(s1, s2-s1-1);
		ret = Cons(str, Null);
		str = Make_String(s2, p-s2);
		Cdr(ret) = str;
	    }
	}
    }
    GC_Unlink;
    return ret;
}

static Object p_parse_triple(Object x) {
    char *p, *ep, *s1, *s2, *s3, delim;
    Object str, ret;
    GC_Node3;

    str = ret = False;
    GC_Link3(x, str, ret);
    Check_Type(x, T_String);
    p = STRING(x)->data;
    ep = p + STRING(x)->size;
    if (p <= ep-4) {
	delim = *p++;
	for (s1 = p; p < ep && *p != delim; p++)
	    ;
	if (p < ep) {
	    for (s2 = ++p; p < ep && *p != delim; p++)
		;
	    if (p < ep) {
		for (s3 = ++p; p < ep && *p != delim; p++)
		    ;
		if (p == ep-1) {
		    str = Make_String(s3, p-s3);
		    ret = Cons(Null, str);
		    str = Make_String(s2, s3-s2-1);
		    Car(ret) = str;
		    ret = Cons(Null, ret);
		    str = Make_String(s1, s2-s1-1);
		    Car(ret) = str;
		}
	    }
	}
    }
    GC_Unlink;
    return ret;
}

static Object p_skip_group(void) {
    Buffer *ip;
    int level = 0;
    char *p, *ep;

    ip = buffer_new(0);
    do {
	if (safe_readline(ip) && ip->size == 0) {
	    warn("end-of-scream while skipping requests");
	    break;
	}
	for (p = ip->data, ep = p + ip->size; p < ep-2; p++)
	    if (*p == escape)
		if (*++p == '{') level++;
		else if (*p == '}') level--;
	buffer_clear(ip);
    } while (level > 0);
    buffer_delete(ip);
    return Void;
}

static Object p_set_escape(Object c) {
    Check_Type(c, T_Character);
    escape = CHAR(c);
    return Void;
}

static Object p_troff_compatible(void) {
    extern int compatible;
    return compatible ? True : False;
}

static Object p_string_prune_left(Object str, Object pref, Object fail) {
    int l1, l2;

    Check_Type(str, T_String);
    Check_Type(pref, T_String);
    l1 = STRING(str)->size, l2 = STRING(pref)->size;
    if (l2 <= l1 && memcmp(STRING(str)->data, STRING(pref)->data, l2) == 0)
	return Make_String(STRING(str)->data+l2, l1-l2);
    return fail;
}

static Object p_string_prune_right(Object str, Object suff, Object fail) {
    int l1, l2, l;

    Check_Type(str, T_String);
    Check_Type(suff, T_String);
    l1 = STRING(str)->size, l2 = STRING(suff)->size, l = l1-l2;
    if (l >= 0 && memcmp(STRING(str)->data+l, STRING(suff)->data, l2) == 0)
	return Make_String(STRING(str)->data, l);
    return fail;
}

static Object p_string_compose(Object old, Object new) {
    Buffer *bp;
    struct S_String *s, *t;
    int i;
    Object ret;

    bp = buffer_new(0);
    Check_Type(old, T_String);
    Check_Type(new, T_String);
    s = STRING(old), t = STRING(new);
    if (t->size > 0) {
	switch (t->data[0]) {
	case '+':
	    buffer_puts(bp, s->data, s->size);
	    buffer_puts(bp, t->data+1, t->size-1);
	    break;
	case '-':
	    for (i = 0; i < s->size; i++)
		if (!memchr(t->data, s->data[i], t->size))
		    buffer_putc(bp, s->data[i]);
	    break;
	default:
	    buffer_puts(bp, t->data, t->size);
	}
    }
    ret = Make_String(bp->data, bp->size);
    buffer_delete(bp);
    return ret;
}

static Object p_repeat_string(Object num, Object str) {
    Buffer *bp;
    Object ret;
    int n;

    Check_Type(str, T_String);
    bp = buffer_new(0);
    for (n = Get_Integer(num); n > 0; n--)
	buffer_puts(bp, STRING(str)->data, STRING(str)->size);
    ret = Make_String(bp->data, bp->size);
    buffer_delete(bp);
    return ret;
}

static Object p_filter_eqn_line(Object str) {
    char *p, *q, *ep;

    Check_Type(str, T_String);
    p = STRING(str)->data, ep = p + STRING(str)->size;
    for ( ; p < ep && isspace(UCHAR(*p)); p++)
	;
    if (p == ep)
	return False;
    for (q = p; p < ep && !isspace(UCHAR(*p)); p++)
	;
    if (p == ep)
	return True;
    if (p-q == 5 && strncmp(q, "delim", 5) == 0) {
	p++;
	if (ep-p == 3 && strncmp(p, "off", 3) == 0)
	    eqn_delim1 = 0;
	else if (ep-p >= 2)
	    eqn_delim1 = *p, eqn_delim2 = p[1];
	return False;
    }
    return p-q == 6 && strncmp(q, "define", 6) == 0 ? False : True;
}

void init_prim(void) {
    error_port = Make_Port (0, stderr, Make_String ("stderr", 6));
    if (setvbuf(stderr, 0, _IOLBF, BUFSIZ) != 0)
	fatal_error("cannot set stderr line buffered");
    Global_GC_Link(error_port);
    Define_Primitive(p_error_port,        "error-port", 0, 0, EVAL);
    Define_Primitive(p_read_line,         "read-line", 0, 0, EVAL);
    Define_Primitive(p_read_line_expand,  "read-line-expand", 0, 0, EVAL);
    Define_Primitive(p_parse,             "parse", 0, MANY, VARARGS);
    Define_Primitive(p_parse_copy_mode,   "parse-copy-mode", 0, MANY, VARARGS);
    Define_Primitive(p_parse_line,        "parse-line", 0, MANY, VARARGS);
    Define_Primitive(p_parse_expand,      "parse-expand", 0, MANY, VARARGS);
    Define_Primitive(p_translate,         "translate", 0, MANY, VARARGS);
    Define_Primitive(p_concat,            "concat", 0, MANY, VARARGS);
    Define_Primitive(p_spread,            "spread", 0, MANY, VARARGS);
    Define_Primitive(p_emit,              "emit", 0, MANY, VARARGS);
    Define_Primitive(p_substitute,        "substitute", 1, MANY, VARARGS);
    Define_Primitive(p_shell_command,     "shell-command", 1, 1, EVAL);
    Define_Primitive(p_remove_file,       "remove-file", 1, 1, EVAL);
    Define_Primitive(p_parse_pair,        "parse-pair", 1, 1, EVAL);
    Define_Primitive(p_parse_triple,      "parse-triple", 1, 1, EVAL);
    Define_Primitive(p_skip_group,        "skip-group", 0, 0, EVAL);
    Define_Primitive(p_set_escape,        "set-escape!", 1, 1, EVAL);
    Define_Primitive(p_troff_compatible,  "troff-compatible?", 0, 0, EVAL);
    Define_Primitive(p_string_prune_left, "string-prune-left", 3, 3, EVAL);
    Define_Primitive(p_string_prune_right,"string-prune-right", 3, 3, EVAL);
    Define_Primitive(p_string_compose,    "string-compose", 2, 2, EVAL);
    Define_Primitive(p_repeat_string,     "repeat-string", 2, 2, EVAL);
    Define_Primitive(p_filter_eqn_line,   "filter-eqn-line", 1, 1, EVAL);
}

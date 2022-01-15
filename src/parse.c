/* $Revision: 1.21 $
 */

/* The troff parser.  Most of the troff-specific code is in this file
 */

#include "unroff.h"

extern int compatible;

char escape = '\\';
char eqn_delim1, eqn_delim2;
static char control = '.';
static char control_nobreak = '\'';
static unsigned char argspec[256];
static char *sentence_end = ".?!";


/* Styles of escape char arguments:
 */
#define ARG_QUOTED    2     /*  \h'xxx', \h|xxx|, ...     */
#define ARG_SYMBOL    4     /*  \fx \f(xx \f[xxx]         */
#define ARG_SIZE      6     /*  \sx \s(xx \s[xxx] \sdd    */
#define ARG_CHAR      8     /*  \zx \z\x \z\(xx \z\[xxx]  */
#define ARG_LINE     16     /*  \"                        */
#define ARG_SIGN      1     /*  optional sign: \s         */


/* parse_char() return values:
 */
#define TOK_CHAR      1
#define TOK_ESC       2


#define skip(c) {\
    for ( ; p < ep && *p != c; p++)\
	;\
    if (p == ep) {\
	warn("missing closing `%s' delimiter", printable_char(c)); return 0;\
    }\
}

#define check_name(c) if (c) {\
    warn("missing escape name"); return 0;\
}

#define check_arg(c) if (c) {\
    warn("missing escape sequence argument"); return 0;\
}

#define check_empty(c) if (c) {\
    warn("empty `[xxx]' sequence"); return 0;\
}

#define is_request(p) \
    ((p)->size > 0 && ((p)->data[0] == control ||\
	(p)->data[0] == control_nobreak ||\
	(p)->data[0] == '.'))


/* Deals with \"  \*  \n.  Returns 0 on error, 1 otherwise.
 */
int parse_expand(Buffer *ip, Buffer *op) {
    char sign, *p, *ep, *q, *s, *ret;
    int len, size_ret, nl, ev, fallback;
    Object str;

    buffer_clear(op);
    for (p = ip->data, ep = p + ip->size; p < ep; ) {
	if (*p == escape && ++p < ep) { 
	    switch (*p) {
	    case '"':
		len = ep-p-1;
		nl = p[len] == '\n';
		args_clear();
		args_add(Make_Char('"'));
		args_add(Make_String(p+1, len));
		ret = event_exec(EV_ESCAPE, "\"", 1, &size_ret, 1);
		if (ret)
		    buffer_puts(op, ret, size_ret);
		else if (nl)
		    buffer_putc(op, '\n');
		p = ep;
		break;
	    case 'n':
	    case '*':
		s = p++;
		check_name(p == ep);
		if (*s == 'n' && (*p == '+' || *p == '-')) {
		    sign = *p++;
		} else sign = 0;
		check_name(p == ep);
		switch (*p) {
		case '(':
		    if ((q = ++p) > ep-2) {
			warn("escape name truncated"); return 0;
		    }
		    len = 2;
		    p += 2;
		    break;
		case '[':
		    if (!compatible) {
			q = ++p;
			skip(']');
			check_empty(p == q);
			len = p++ - q;
			break;
		    }
		default:
		    check_name((q = p++) == ep);
		    len = 1;
		    break;
		}
		str = Make_String(q, len);
		ev = *s == 'n' ? EV_NUMREG : EV_STRING;
		fallback = event_lookup(EV_ESCAPE, s, 1) &&
		    !event_lookup(ev, q, len);
		args_clear();
		if (fallback)
		    args_add(Make_Char(*s));
		args_add(str);
		if (sign)
		    args_add(Make_Char(sign));
		if (fallback)
		    ret = event_exec(EV_ESCAPE, s, 1, &size_ret, 1);
		else
		    ret = event_exec(ev, q, len, &size_ret, 1);
		if (ret)
		    buffer_puts(op, ret, size_ret);
		else
		    buffer_puts(op, s-1, p-s+1);
		break;
	    default:
		buffer_putc(op, escape);
		buffer_putc(op, *p);
		p++;
		break;
	    }
	} else if (isspace(UCHAR(*p))) {    /* kill space before comment */
	    for (s = p+1; s < ep-1 && isspace(UCHAR(*s)); s++)
		;
	    if (s < ep-1 && *s == escape && s[1] == '"')
		p = s;
	    else buffer_putc(op, *p++);
	} else buffer_putc(op, *p++);
    }
    return 1;
}

static int parse_char(char **pp, char *ep, Buffer *op,
	int doescape, int doexec, int copymode) {
    char sign, *p, *s, *ret;
    char *q = 0;                        /* make gcc -Wuninitialized happy */
    int size_ret, tok = 0, len = 0, nl = 0; /* ditto */
    unsigned char spec;

    p = *pp;
    if (*p == escape && doescape && p < ep-1) {
	if (copymode) {
	    if (p[1] != '$') {
		if (doexec && p[1] != '.' && p[1] != '\\')
		    buffer_putc(op, *p);
		if (doexec)
		    buffer_putc(op, p[1]);
		*pp = p+2;
		return TOK_CHAR;
	    }
	}
	switch (*++p) {
	case '(':
	    if (++p > ep-2) {
		warn("special character truncated"); return 0;
	    }
	    if (doexec) {
		args_clear();
		args_add(Make_String(p, 2));
		if ((ret = event_exec_fallback(EV_SPECIAL, p, 2,
			&size_ret)) == 0)
		    buffer_puts(op, p-2, 4);
		else
		    buffer_puts(op, ret, size_ret);
	    }
	    *pp = p+2;
	    return TOK_CHAR;
	case '[':
	    if (!compatible) {
		q = ++p;
		skip(']');
		check_empty(p == q);
		if (doexec) {
		    args_clear();
		    args_add(Make_String(q, p-q));
		    if ((ret = event_exec_fallback(EV_SPECIAL, q, p-q,
			    &size_ret)) == 0)
			buffer_puts(op, q-2, p-q+3);
		    else
			buffer_puts(op, ret, size_ret);
		}
		*pp = p+1;
		return TOK_CHAR;
	    }
	default:
	    spec = argspec[(unsigned char)*p];
	    s = p++;
	    if (spec & ARG_SIGN && p < ep && (*p == '+' || *p == '-')) {
		sign = *p++;
	    } else sign = 0;
	    switch (spec &= ~ARG_SIGN) {
	    case ARG_SYMBOL:
	    case ARG_SIZE:
		check_arg(p == ep);
		switch (*p) {
		case '(':
		    if ((q = ++p) > ep-2) {
			warn("escape sequence argument truncated");
			return 0;
		    }
		    len = 2;
		    p += 2;
		    break;
		case '[':
		    if (!compatible) {
			q = ++p;
			skip(']');
			check_empty(p == q);
			len = p++ - q;
			break;
		    }
		default:
		    check_arg((q = p++) == ep);
		    len = 1;
		    if (spec == ARG_SIZE && q < ep-1 && *q > '0' && *q < '4'
			    && isdigit(UCHAR(*p))) {
			len++, p++;
		    }
		    break;
		}
		break;
	    case ARG_CHAR:
		check_arg((q = p) == ep);
		if (parse_char(&p, ep, op, doescape, 0, copymode) == 0)
		    return 0;
		len = p - q;
		break;
	    case ARG_LINE:
		q = p;
		p = ep;
		len = ep - q;
		nl = ep[-1] == '\n';
		break;
	    case ARG_QUOTED:
		if ((q = p++) >= ep) {
		    warn("missing opening delimiter"); return 0;
		}
		if (p == ep) {
undelim:            warn("missing closing `%s' delimiter", printable_char(*q));
		    return 0;
		}

		while ((tok = parse_char(&p, ep, op, doescape, 0, copymode))
			!= 0) {
		    if (tok == TOK_CHAR && p[-1] == *q)
			break;
		    if (p == ep)
			goto undelim;
		}
		if (tok == 0)
		    return 0;
		len = p - ++q - 1;
		break;
	    }
	    if (doexec) {
		args_clear();
		args_add(Make_Char(*s));
		if (spec) {
		    args_add(Make_String(q, len));
		    if (sign)
			args_add(Make_Char(sign));
		}
		ret = event_exec_fallback(EV_ESCAPE, s, 1, &size_ret);
		if (ret)
		    buffer_puts(op, ret, size_ret);
		else if (spec & ARG_LINE) {
		    if (nl) buffer_putc(op, '\n');
		} else
		    buffer_puts(op, s-1, p-s+1);
	    }
	    *pp = p;
	    return TOK_ESC;
	}
    } else if (doexec && doescape && !copymode &&
	    eqn_delim1 && *p == eqn_delim1) {
	s = ++p;
	for ( ; p < ep && *p != eqn_delim2; p++)
	    ;
	if (p == ep) {
	    warn("non-terminated inline equation");
	    return 0;
	}
	args_clear();
	args_add(Make_String(s, p-s));
	ret = event_exec(EV_EQUATION, 0, 0, &size_ret, 0);
	if (ret)
	    buffer_puts(op, ret, size_ret);
	else
	    buffer_puts(op, s-1, p-s+2);
	*pp = p+1;
	return TOK_ESC;
    } else if (doexec && !copymode && event_lookup(EV_CHAR, p, 1)) {
	args_clear();
	args_add(Make_Char(*p));
	ret = event_exec(EV_CHAR, p, 1, &size_ret, 0);
	if (ret)
	    buffer_puts(op, ret, size_ret);
	else
	    buffer_putc(op, *p);
	*pp = p+1;
	return TOK_CHAR;
    } else {
	if (doexec)
	    buffer_putc(op, *p);
	*pp = p+1;
	return TOK_CHAR;
    }
}

/* Deals with \(  \[  \x  and chardefs.  Returns 0 on error, 1 otherwise.
 */
int parse_escape(Buffer *ip, Buffer *op, int doescape, int copymode) {
    char *p, *ep;

    buffer_clear(op);
    p = ip->data;
    ep = p + ip->size;
    while (p < ep && parse_char(&p, ep, op, doescape, 1, copymode) != 0)
	;
    return p != 0;
}

static int parse_request(Buffer *ip, Buffer *op) {
    char *p, *q, *t, *ep, *ret, *start;
    int reqlen, quote, len, size_ret;
    int num, macro, minargs = 0, maxargs = 0;
    Elem *event;
    Object obj;

    assert(ip->size > 0);
    p = ip->data+1;
    ep = ip->data+ip->size;
    for ( ; p < ep && (*p == ' ' || *p == '\t'); p++)
	;

    start = p;
    for ( ; p < ep && !isspace(UCHAR(*p)); p++)
	if (compatible && p == start+2)
	    break;
    if ((reqlen = p - start) == 0)    /* just a period */
	return 0;

    if ((event = event_lookup(EV_MACRO, start, reqlen)) != 0) {
	macro = 1;
    } else if ((event = event_lookup(EV_REQUEST, start, reqlen)) != 0) {
	macro = 0;
	obj = get_object(event->obj);
	/*
	 * The following three lines prevent what I believe is an optimizer
	 * bug under OSF/1.  `obj' is invalid unless it is passed to an
	 * (arbitrary) function before entering the if-statement.
	 */
#ifdef __osf__
	(void)P_Not(obj);       /* any function... */
#endif
	if (TYPE(obj) == T_Compound) {
	    if ((maxargs = COMPOUND(obj)->max_args - 1) < 0)
		maxargs = INT_MAX;
	    minargs = COMPOUND(obj)->min_args;
	}
    } else {
	warn("no event value for request or macro `%s'",
	    printable_string(start, reqlen));
	return 0;
    }

    args_clear();
    args_add(Make_String(start, reqlen));
    for (num = 1; ; num++) {
	for ( ; p < ep && isspace(UCHAR(*p)); p++)
	    ;
	if (p >= ep)
	    break;
	if (macro && *p == '"') {
	    quote = 1;
	    p++;
	} else quote = 0;
	q = t = p;
	while (1) {
	    if (p == ep || *p == '\n') {
		/* If quote==1, a closing delimiter is missing.  This is
		 * not regarded as an error in troff.
		 */
		break;
	    }
	    if (!quote && isspace(UCHAR(*p)) && (macro || num < maxargs))
		break;
	    if (quote && *p == '"') {
		if (p < ep-1 && p[1] == '"')    /* turn "" into " */
		    p++;
		else
		    break;
	    }
	    if (*p == escape && p < ep-1) {
		if (p[1] != '\\')
		    *t++ = *p;
		p++;
	    }
	    *t++ = *p++;
	}
	p++;
	len = t-q;
	args_add(Make_String(q, len));
    }
    if (!macro)
	while (num++ < minargs)
	    args_add(Make_String("", 0));
    ret = event_exec(macro ? EV_MACRO : EV_REQUEST, start, reqlen,
	&size_ret, 1);
    if (ret) {
	buffer_clear(op);
	buffer_puts(op, ret, size_ret);
    }
    return ret != 0;
}

static int is_sentence_end(Buffer *bp) {
    int len = bp->size;

    return len > 1 && bp->data[len-1] == '\n' &&
	strchr(sentence_end, bp->data[len-2]);
}

static void exec_sentence_end(Buffer *bp) {
    int size_ret;
    char *ret;

    args_clear();
    args_add(Make_Char(bp->data[bp->size-2]));
    if ((ret = event_exec(EV_SENTENCE, 0, 0, &size_ret, 0)) != 0) {
	bp->size -= 2;
	buffer_puts(bp, ret, size_ret);
    }
}

static void exec_line(Buffer *bp) {
    args_clear();
    if (bp->size > 0)
	args_add(Make_Char(bp->data[bp->size-1]));
    else
	args_add(False);
    events_vec_exec(EV_LINE);
}

void parse_line(Buffer *ip, Buffer *op) {
    int do_sentence_end;

    if (is_request(ip)) {
	if (!parse_request(ip, op))
	    return;
	if (op->size == 0)    /* it's not considered an input line */
	    return;
	do_sentence_end = 1;
    } else {
	do_sentence_end = is_sentence_end(ip);
	if (!parse_escape(ip, op, 1, 0))
	    return;
    }
    if (do_sentence_end && is_sentence_end(op))
	exec_sentence_end(op);
    safe_write(op->data, op->size);
    exec_line(op);
}

void parse_input(void) {
    int eof_seen;
    Buffer *b1, *b2;

    b1 = buffer_new(0);
    b2 = buffer_new(0);
    do {
	buffer_clear(b1);
	if ((eof_seen = safe_readline(b1)) && b1->size == 0)
	    break;
	if (parse_expand(b1, b2))
	    parse_line(b2, b1);
    } while (!eof_seen);
    buffer_delete(b1);
    buffer_delete(b2);
}

void init_parse(void) {
    argspec['b'] = ARG_QUOTED;
    argspec['c'] = ARG_CHAR;
    argspec['f'] = ARG_SYMBOL;
    argspec['h'] = ARG_QUOTED;
    argspec['k'] = ARG_SYMBOL;
    argspec['l'] = ARG_QUOTED;
    argspec['n'] = ARG_SYMBOL|ARG_SIGN;
    argspec['o'] = ARG_QUOTED;
    argspec['s'] = ARG_SIZE|ARG_SIGN;
    argspec['v'] = ARG_QUOTED;
    argspec['w'] = ARG_QUOTED;
    argspec['x'] = ARG_QUOTED;
    argspec['z'] = ARG_CHAR;
    argspec['*'] = ARG_SYMBOL;
    argspec['$'] = ARG_SYMBOL;
    argspec['"'] = ARG_LINE;
    if (!compatible) {
	argspec['A'] = ARG_QUOTED;
	argspec['C'] = ARG_QUOTED;
	argspec['L'] = ARG_QUOTED;
	argspec['N'] = ARG_QUOTED;
	argspec['R'] = ARG_QUOTED;
	argspec['V'] = ARG_SYMBOL;
	argspec['Y'] = ARG_SYMBOL;
	argspec['Z'] = ARG_QUOTED;
    }
}

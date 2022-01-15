/* $Revision: 1.15 $
 */

/* Scheme primitives that define and query event procedures, and
 * functions to lookup and execute events.
 */

#include "unroff.h"

#define NUM_VEC_EVENTS   6

static Table *requests, *numregs, *specials, *escapes, *chars, *others;
static Table *fallbacks;
static Object Key_Equation, Key_Sentence;
Object eventsvec;

static SYMDESCR event_syms[] = {
    { "line",      EV_LINE },
    { "prolog",    EV_PROLOG },
    { "epilog",    EV_EPILOG },
    { "option",    EV_OPTION },
    { "start",     EV_START },
    { "exit",      EV_EXIT },
    { 0, 0 }
};

static char *event_names[] = {
    "line event",
    "prolog event",
    "epilog event",
    "option event",
    "start event",
    "exit event",
    "request",
    "macro",
    "string",
    "number register",
    "special character",
    "inline equation",
    "sentence event",
    "escape sequence",
    "character event",
};

static Object make_event_object(Object x) {
    switch (TYPE(x)) {
    case T_Null:
    case T_Compound:
    case T_String:
    case T_Character:
    case T_Symbol:
	return x;
    case T_Primitive:
	Primitive_Error("event function must be a compound procedure");
    default:
	if (EQ(x, False))
	    return x;
	Primitive_Error("invalid event value argument");
    }
}

static Object store_event(Table *tp, char *key, int size, Object obj,
	char flags) {
    Elem *oldp;
    Object ret = False;

    if ((oldp = table_lookup(tp, key, size)) != 0)
	ret = get_object(oldp->obj);
    if (!Nullp(obj))
	if (EQ(obj, False))
	    table_remove(tp, key, size);
	else
	    table_store(tp, key, size, obj, (unsigned long)flags);
    return ret;
}

static Object def_indexed(Table *tp, Object key, Object obj, char flags,
	char *code) {
    if (TYPE(key) == T_Symbol)
	key = SYMBOL(key)->name;
    else if (TYPE(key) != T_String)
	Wrong_Type_Combination(key, "string or symbol");
    if (STRING(key)->size == 0) {
	if (*code != 'r' && *code != 'p')
	    Primitive_Error("event key must be of non-zero length");
	return store_event(fallbacks, code, 1, make_event_object(obj), flags);
    }
    return store_event(tp, STRING(key)->data, STRING(key)->size,
	make_event_object(obj), flags);
}

static Object p_defrequest(Object key, Object obj) {
    return def_indexed(requests, key, obj, 0, "r");
}

static Object p_defmacro(Object key, Object obj) {
    return def_indexed(requests, key, obj, RQ_MACRO, "m");
}

static Object p_defstring(Object key, Object obj) {
    return def_indexed(requests, key, obj, RQ_STRING, "s");
}

static Object p_defnumreg(Object key, Object obj) {
    return def_indexed(numregs, key, obj, 0, "n");
}

static Object p_defspecial(Object key, Object obj) {
    return def_indexed(specials, key, obj, 0, "p");
}

static Object p_defequation(Object obj) {
    return def_indexed(others, Key_Equation, obj, 0, 0);
}

static Object p_defsentence(Object obj) {
    return def_indexed(others, Key_Sentence, obj, 0, 0);
}

static Object p_requestdef(Object key) {
    return def_indexed(requests, key, Null, 0, "r");
}

static Object p_macrodef(Object key) {
    return def_indexed(requests, key, Null, 0, "m");
}

static Object p_stringdef(Object key) {
    return def_indexed(requests, key, Null, 0, "s");
}

static Object p_numregdef(Object key) {
    return def_indexed(numregs, key, Null, 0, "n");
}

static Object p_specialdef(Object key) {
    return def_indexed(specials, key, Null, 0, "p");
}

static Object p_equationdef(void) {
    return def_indexed(others, Key_Equation, Null, 0, 0);
}

static Object p_sentencedef(void) {
    return def_indexed(others, Key_Sentence, Null, 0, 0);
}

static Object def_char_event(Table *tp, Object key, Object obj) {
    char c;

    switch (TYPE(key)) {
    case T_Character:
	c = CHAR(key); break;
    case T_Symbol:
	key = SYMBOL(key)->name;
	/* fall through */
    case T_String:
	if (tp == escapes && STRING(key)->size == 0)
	    return store_event(fallbacks, "e", 1, make_event_object(obj), 0);
	if (STRING(key)->size != 1)
	    goto err;
	c = STRING(key)->data[0];
	break;
    default: err:
	Primitive_Error("cannot coerce argument to character");
    }
    return store_event(tp, &c, 1, make_event_object(obj), 0);
}

Object p_defescape(Object key, Object obj) {
    return def_char_event(escapes, key, obj);
}

Object p_defchar(Object key, Object obj) {
    return def_char_event(chars, key, obj);
}

Object p_escapedef(Object key) {
    return def_char_event(escapes, key, Null);
}

Object p_chardef(Object key) {
    return def_char_event(chars, key, Null);
}

Elem *event_lookup(Event e, char *key, int size) {
    Elem *p;

    if (key && size == 0) {
	switch(e) {
	case EV_REQUEST:
	    return table_lookup(fallbacks, "r", 1);  /* not yet */
	case EV_MACRO:
	    return table_lookup(fallbacks, "m", 1);  /* not yet */
	case EV_SPECIAL:
	    return table_lookup(fallbacks, "p", 1);
	case EV_ESCAPE:
	    return table_lookup(fallbacks, "e", 1);
	default:
	    assert(0);
	}
    } else {
	switch (e) {
	case EV_REQUEST:
	    return ((p = table_lookup(requests, key, size)) &&
		!(p->flags & RQ_MACRO)) ? p : 0;
	case EV_MACRO:
	    return ((p = table_lookup(requests, key, size)) &&
		(p->flags & RQ_MACRO)) ? p : 0;
	case EV_STRING:
	    return table_lookup(requests, key, size);
	case EV_NUMREG:
	    return table_lookup(numregs, key, size);
	case EV_SPECIAL:
	    return table_lookup(specials, key, size);
	case EV_ESCAPE:
	    return table_lookup(escapes, key, size);
	case EV_CHAR:
	    assert(size == 1);
	    return table_lookup(chars, key, size);
	case EV_EQUATION:
	    return table_lookup(others, "e", 1);
	case EV_SENTENCE:
	    return table_lookup(others, "s", 1);
	case EV_LINE:
	case EV_PROLOG:
	case EV_EPILOG:
	case EV_OPTION:
	case EV_START:
	case EV_EXIT:
	default:
	    assert(0);
	}
    }
    return 0;    /* shut up gcc -W */
}

static int check_num_args(Object fun, int num, char *evnam) {
    char *s = 0;
    struct S_Compound *comp = COMPOUND(fun);

    if (num < comp->min_args)
	s = "few";
    else if (comp->max_args >= 0 && num > comp->max_args)
	s = "many";
    if (s) warn("too %s arguments for %s function", s, evnam);
    return !s;
}

char *event_exec(Event e, char *key, int size, int *size_ret, int complain) {
    Elem *p = event_lookup(e, key, size);
    Object ret;
    char *name = event_names[e];
    static char c;

    if (p) {
	if ((e == EV_REQUEST || e == EV_MACRO) && (p->flags & RQ_STRING)) {
	    warn("cannot execute string as request or macro");
	    return 0;
	}
	if (e == EV_STRING && !(p->flags & RQ_STRING)) {
	    warn("cannot execute request or macro as string");
	    return 0;
	}
	ret = get_object(p->obj);
	if (TYPE(ret) == T_Compound) {
	    if (!check_num_args(ret, args_num(), name))
		return 0;
	    ret = Funcall(ret, args_get(), 0);
	}
	assert(size_ret != 0);
	switch (TYPE(ret)) {
	case T_Character:
	    c = CHAR(ret);
	    *size_ret = 1;
	    return &c;
	case T_Symbol:
	    ret = SYMBOL(ret)->name;   /* fall through */
	case T_String:
	    *size_ret = STRING(ret)->size;
	    return STRING(ret)->data;
	default:
	    warn("cannot coerce result of %s function to string", name);
	    return 0;
	}
    }
    if (complain)
	warn("no event value for %s `%s'", name, printable_string(key, size));
    return 0;
}

char *event_exec_fallback(Event e, char *key, int size, int *size_ret) {
    if (!event_lookup(e, key, size) && event_lookup(e, "", 0))
	return event_exec(e, "", 0, size_ret, 1);
    else
        return event_exec(e, key, size, size_ret, 1);
}

static Object store_vec_event(int e, Object pri, Object obj) {
    Object ret, v = VECTOR(eventsvec)->data[e];
    int p = Get_Integer(pri);

    if (p < 0 || p > 99)
	Range_Error(pri);
    ret = VECTOR(v)->data[p];
    if (!Nullp(obj))
	VECTOR(v)->data[p] = obj;
    return ret;
}

static Object p_defevent(Object event, Object pri, Object obj) {
    char e = Symbols_To_Bits(event, 0, event_syms);

    if (Truep(obj) && TYPE(obj) != T_Compound)
	Wrong_Type_Combination(obj, "compound procedure or #f");
    return store_vec_event(e, pri, obj);
}

static Object p_eventdef(Object event, Object pri) {
    char e = Symbols_To_Bits(event, 0, event_syms);

    return store_vec_event(e, pri, Null);
}

void events_vec_exec(Event e) {
    Object func, v;
    int i;

    for (i = 0; i < 100; i++) {
	v = VECTOR(eventsvec)->data[e];
	func = VECTOR(v)->data[i];
	if (Truep(func) && check_num_args(func, args_num(), event_names[e]))
	    (void)Funcall(func, args_get(), 0);
    }
}

void init_event(void) {
    int i;

    requests = table_new(256*256);
    numregs = table_new(256*256);
    specials = table_new(256*256);
    escapes = table_new(256);
    chars = table_new(256);
    others = table_new(10);
    fallbacks = table_new(256);
    Key_Equation = Make_String("e", 1);
    Global_GC_Link(Key_Equation);
    Key_Sentence = Make_String("s", 1);
    Global_GC_Link(Key_Sentence);
    eventsvec = Make_Vector(NUM_VEC_EVENTS, Null);
    Global_GC_Link(eventsvec);
    for (i = 0; i < NUM_VEC_EVENTS; i++)
	VECTOR(eventsvec)->data[i] = Make_Vector(100, False);
    Define_Primitive(p_defrequest,  "defrequest", 2, 2, EVAL);
    Define_Primitive(p_defmacro,    "defmacro", 2, 2, EVAL);
    Define_Primitive(p_defstring,   "defstring", 2, 2, EVAL);
    Define_Primitive(p_defnumreg,   "defnumreg", 2, 2, EVAL);
    Define_Primitive(p_defspecial,  "defspecial", 2, 2, EVAL);
    Define_Primitive(p_defequation, "defequation", 1, 1, EVAL);
    Define_Primitive(p_defsentence, "defsentence", 1, 1, EVAL);
    Define_Primitive(p_defescape,   "defescape", 2, 2, EVAL);
    Define_Primitive(p_defchar,     "defchar", 2, 2, EVAL);
    Define_Primitive(p_defevent,    "defevent", 3, 3, EVAL);
    Define_Primitive(p_requestdef,  "requestdef", 1, 1, EVAL);
    Define_Primitive(p_macrodef,    "macrodef", 1, 1, EVAL);
    Define_Primitive(p_stringdef,   "stringdef", 1, 1, EVAL);
    Define_Primitive(p_numregdef,   "numregdef", 1, 1, EVAL);
    Define_Primitive(p_specialdef,  "specialdef", 1, 1, EVAL);
    Define_Primitive(p_equationdef, "equationdef", 0, 0, EVAL);
    Define_Primitive(p_sentencedef, "sentencedef", 0, 0, EVAL);
    Define_Primitive(p_escapedef,   "escapedef", 1, 1, EVAL);
    Define_Primitive(p_chardef,     "chardef", 1, 1, EVAL);
    Define_Primitive(p_eventdef,    "eventdef", 2, 2, EVAL);
}

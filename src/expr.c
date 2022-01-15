/* $Revision: 1.5 $
 */

/* Scheme primitives that deal with troff numeric expressions.
 */

#include "unroff.h"

#define SCALE_INDICATORS   "icPmnpuv"

enum operator {
    ADD, SUB, DIV, MUL, MOD, LT, GT, LE, GE, EQ, AND, OR, OOPS
};

static int scale_factor[128];
static int scale_divisor[128];

static Object p_set_scaling(Object scale, Object x, Object y) {
    int d, c;

    Check_Type(scale, T_Character);
    c = CHAR(scale);
    if (c == 0 || strchr(SCALE_INDICATORS, c) == 0)
	Primitive_Error("invalid scale indicator ~s", scale);
    scale_factor[c] = Get_Integer(x);
    if ((d = Get_Integer(y)) == 0)
	Range_Error(y);
    scale_divisor[c] = d;
    return Void;
}

static Object p_get_scaling(Object scale) {
    int c;
    Object ret = Null;
    GC_Node;

    Check_Type(scale, T_Character);
    c = CHAR(scale);
    if (c == 0 || strchr(SCALE_INDICATORS, c) == 0)
	Primitive_Error("invalid scale indicator ~s", scale);
    GC_Link(ret);
    ret = Cons(Make_Integer(scale_factor[c]), Null);
    Cdr(ret) = Make_Integer(scale_divisor[c]);
    GC_Unlink;
    return ret;
}

static enum operator get_operator(char **p) {
    enum operator op;

    switch (**p) {
    case '+':
	op = ADD; break;
    case '-':
	op = SUB; break;
    case '/':
        op = DIV; break;
    case '*':
	op = MUL; break;
    case '%':
	op = MOD; break;
    case '<':
	if ((*p)[1] == '=') {
	    (*p)++;
	    op = LE;
	} else
	    op = LT;
	break;
    case '>':
	if ((*p)[1] == '=') {
	    (*p)++;
	    op = GE;
	} else
	    op = GT;
	break;
    case '&':
	op = AND; break;
    case ':':
	op = OR;  break;
    case '=':
	if ((*p)[1] == '=')
	    (*p)++;
	op = EQ;
	break;
    default:
	return OOPS;
    }
    (*p)++;
    return op;
}

static double parse_expr(char **, int, int);

static double get_operand(char **p, int scale, int rest) {
    double d;
    char *ep;

    if (**p == '(') {
	(*p)++;
	d = parse_expr(p, scale, rest);
	if (*p) {
	    if (**p != ')') *p = 0;
	    else (*p)++;
	}
	return d;
    }
    d = strtod(*p, &ep);
    if (ep == *p) {
	*p = 0;
	return 0;
    }
    *p = ep;
    if (**p && strchr(SCALE_INDICATORS, **p)) {
	scale = **p;
	(*p)++;
    }
    return d * scale_factor[scale] / scale_divisor[scale];
}

static double parse_expr(char **p, int scale, int rest) {
    double acc, d;
    enum operator op;

    acc = get_operand(p, scale, rest);
    if (*p == 0)
	return 0;
    while (**p && **p != ')') {
	if ((op = get_operator(p)) == OOPS) {
	    if (rest)
		break;
err:        *p = 0;
	    return 0;
	}
	d = get_operand(p, scale, rest);
	if (*p == 0)
	    return 0;
	switch (op) {
	case ADD:
	    acc += d; break;
	case SUB:
	    acc -= d; break;
	case DIV:
	    if (d == 0.0) {
		warn("division by zero"); goto err;
	    }
	    acc /= d; break;
	case MUL:
	    acc *= d; break;
	case MOD:
	    acc = fmod(acc, d);
	    if (isnan(acc)) {
		warn("division by zero"); goto err;
	    }
	    break;
	case LT:
	    acc = acc < d; break;
	case GT:
	    acc = acc > d; break;
	case LE:
	    acc = acc <= d; break;
	case GE:
	    acc = acc >= d; break;
	case EQ:
	    acc = acc == d; break;
	case AND:
	    acc = acc > 0 && d > 0; break;
	case OR:
	    acc = acc > 0 || d > 0; break;
	case OOPS:
	    assert(0);
	}
    }
    if (!finite(acc)) {
	warn("expression evaluates to infinity"); goto err;
    }
    return acc;
}

static Object parse_expression(Object str, Object fail, Object scale,
	int rest) {
    int c;
    char *e, *s;
    double d;
    Object ret;

    Check_Type(scale, T_Character);
    c = CHAR(scale);
    if (c == 0 || strchr(SCALE_INDICATORS, c) == 0)
	Primitive_Error("invalid scale indicator ~s", scale);
    e = s = Get_String(str);
    d = parse_expr(&s, c, rest);
    if (s == 0 || (*s && !rest)) {
	warn("invalid expression: `%s'", e);
	return fail;
    }
    ret = P_Inexact_To_Exact(Make_Flonum(d));
    if (rest) {
	Object x;
	GC_Node;

	GC_Link(ret);
	ret = Cons(ret, Null);
	x = Make_String(s, strlen(s));
	Cdr(ret) = x;
	GC_Unlink;
    }
    return ret;
}

static Object p_parse_expression(Object str, Object fail, Object scale) {
    return parse_expression(str, fail, scale, 0);
}

static Object p_parse_expression_rest(Object str, Object fail, Object scale) {
    return parse_expression(str, fail, scale, 1);
}

static Object p_char_expression_delimiter(Object c) {
    Check_Type(c, T_Character);
    return strchr("0123456789()+-*/%<>=:&.", CHAR(c)) ? True : False;
}

void init_expr(void) {
    char *p;

    for (p = SCALE_INDICATORS; *p; p++)
	scale_factor[(int)*p] = scale_divisor[(int)*p] = 1;
    Define_Primitive(p_set_scaling,      "set-scaling!", 3, 3, EVAL);
    Define_Primitive(p_get_scaling,      "get-scaling", 1, 1, EVAL);
    Define_Primitive(p_parse_expression, "parse-expression", 3, 3, EVAL);
    Define_Primitive(p_parse_expression_rest,
				"parse-expression-rest", 3, 3, EVAL);
    Define_Primitive(p_char_expression_delimiter,
				"char-expression-delimiter?", 1, 1, EVAL);
}

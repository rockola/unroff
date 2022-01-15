/* $Revision: 1.5 $
 */

/* Functions that maintain a properly GC-linked list of Scheme
 * arguments.  The parser collects arguments to an event procedure
 * using args_add(); the code that eventually calls the event
 * procedure calls args_get() to obtain a list of arguments that
 * can directly be passed to Funcall().
 *
 * args_clear()  --  clears the list
 * args_add(obj) --  appends an object to the list
 * args_get()    --  returns the list
 * args_num()    --  returns the current length of the list
 */

#include "unroff.h"

static Object args, last, rest;
static int num;

void args_clear(void) {
    if (Nullp(last)) {
	if (!Nullp(rest))
	    args = rest;
    } else
	P_Setcdr(last, rest);
    last = Null;
    num = 0;
}

void args_add(Object x) {
    Object tmp;
    GC_Node;

    if (Nullp(last)) {
	last = args;
    } else {
	if (Nullp(Cdr(last))) {
	    GC_Link(x);
	    tmp = Cons(False, Null);
	    GC_Unlink;
	    Cdr(last) = tmp;
	}
	last = Cdr(last);
    }
    Car(last) = x;
    rest = Cdr(last);
    num++;
}


Object args_get(void) {
    if (Nullp(last)) {
	rest = args;
	return Null;
    }
    rest = Cdr(last);
    P_Setcdr(last, Null);
    return args;
}

int args_num(void) {
    return num;
}

void init_args(void) {
    rest = Cons(False, Null);
    args = last = Null;
    Global_GC_Link(args);
    Global_GC_Link(last);
    Global_GC_Link(rest);
}

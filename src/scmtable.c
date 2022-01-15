/* $Revision: 1.3 $
 */

/* The implementation of the Scheme type `table' and the primitives
 * that work on tables.  This file is basically an additional layer
 * on top of the code in table.c.
 */

#include "unroff.h"

#define TABLE(x)     ((struct s_table *)POINTER(x))

struct s_table {
    Object tag;
    Table *t;
};

static int T_Table;

static Object p_tablep(Object x) {
    return TYPE(x) == T_Table ? True : False;
}

static int table_equal(Object t1, Object t2) {
    return EQ(t1, t2);
}

static int table_print(Object x, Object port, int raw, int depth,
	int length) {
    Printf(port, "#[table %lu]", TABLE(x)->t);
    return 0;
}

static Object terminate_table(Object x) {
    table_delete(TABLE(x)->t);
    return Void;
}

static Object p_make_table(Object size) {
    Object t;
    int s;

    if ((s = Get_Integer(size)) <= 0)
	Range_Error(size);
    t = Alloc_Object(sizeof(struct s_table), T_Table, 0);
    TABLE(t)->tag = Null;
    TABLE(t)->t = table_new(s);
    Register_Object(t, (GENERIC)0, terminate_table, 0);
    return t;
}

static Object table_op(int op, Object t, Object key, Object val) {
    Elem *p;
    Object ret = Void;
    char *data;
    int size;
    Table *tp;

    Check_Type(t, T_Table);
    tp = TABLE(t)->t;
    if (TYPE(key) == T_Symbol)
	key = SYMBOL(key)->name;
    else if (TYPE(key) != T_String)
	Wrong_Type_Combination(key, "string or symbol");
    data = STRING(key)->data;
    size = STRING(key)->size;
    if (size == 0)
	Primitive_Error("key must be of non-zero length");
    switch(op) {
    case 's':
	table_store(tp, data, size, val, 0);
	break;
    case 'r':
	table_remove(tp, data, size);
	break;
    case 'l':
	if ((p = table_lookup(tp, data, size)) == 0)
	    ret = False;
	else
	    ret = get_object(p->obj);
    }
    return ret;
}

static Object p_table_store(Object t, Object key, Object val) {
    return table_op('s', t, key, val);
}

static Object p_table_remove(Object t, Object key) {
    return table_op('r', t, key, Null);
}

static Object p_table_lookup(Object t, Object key) {
    return table_op('l', t, key, Null);
}

void init_scmtable(void) {
    T_Table = Define_Type(0, "table", NOFUNC, sizeof(struct s_table),
	table_equal, table_equal, table_print, NOFUNC);
    Define_Primitive(p_tablep,       "table?", 1, 1, EVAL);
    Define_Primitive(p_make_table,   "make-table", 1, 1, EVAL);
    Define_Primitive(p_table_store,  "table-store!", 3, 3, EVAL);
    Define_Primitive(p_table_remove, "table-remove!", 2, 2, EVAL);
    Define_Primitive(p_table_lookup, "table-lookup", 2, 2, EVAL);
}

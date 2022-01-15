/* $Revision: 1.5 $
 */

#include "unroff.h"

/* Functions to register Scheme objects in a vector that has been added
 * to the GC root set to avoid having to deal with weak pointers.
 * Code using these functions works with indexes into the vector.
 *
 * register_object(obj)    --  registers an object and returns index
 * deregister_object(inx)  --  deregisters object specified by index
 * get_object(inx)         --  returns object specified by index
 */

static int max_objects = 32;     /* initial size */
static int num_objects;
static Object objects;
static int inx;

int register_object(Object x) {
    Object v;
    int n;
    GC_Node;

    if (num_objects == max_objects) {
	max_objects *= 2;
	GC_Link(x);
	v = Make_Vector(max_objects, Null);
	GC_Unlink;
	memcpy(VECTOR(v)->data, VECTOR(objects)->data,
	    num_objects * sizeof(Object));
	objects = v;
	inx = num_objects;
    }
    for (n = 0; !Nullp(VECTOR(objects)->data[inx]);
	    inx++, inx %= max_objects) {
	n++;
	assert(n < max_objects);
    }
    VECTOR(objects)->data[inx] = x;
    num_objects++;
    return inx;
}

void deregister_object(int i) {
    VECTOR(objects)->data[i] = Null;
    --num_objects;
    assert(num_objects >= 0);
}

Object get_object(int i) {
    return VECTOR(objects)->data[i];
}

void init_gcroot(void) {
    objects = Make_Vector(max_objects, Null);
    Global_GC_Link(objects);
}

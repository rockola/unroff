/* $Revision: 1.8 $
 */

/* Simple hash tables.
 *
 * table_new(size)             --  returns a new hash table
 * table_delete(t)             --  frees a table and its entries
 * table_store(t,key,keylen,obj,flags)
 *                             --  stores Scheme object and flags under key
 * table_lookup(t,key,keylen)  --  returns object stored under key, or 0
 * table_remove(t,key,keylen)  --  removes entry stored under key
 */


#include "unroff.h"

Table *table_new(int size) {
    Table *p;
    int i;

    assert(size > 0);
    p = safe_malloc(sizeof *p);
    p->data = safe_malloc(size * sizeof(Elem *));
    for (i = 0; i < size; i++)
	p->data[i] = 0;
    p->size = size;
    return p;
}

void table_delete(Table *tp) {
    int i;
    Elem *p, *q;

    for (i = 0; i < tp->size; i++)
	for (p = tp->data[i]; p; p = q) {
	    deregister_object(p->obj);
	    free(p->data);
	    q = p->next;
	    free(p);
	}
    free(tp->data);
    free(tp);
}

/* This function ensures that no collisions can occur in tables of
 * size 256^N if all keys are of length <= N (for small values of N).
 */
static unsigned long hash(char *key, int size) {
    unsigned long i, j;

    assert(size > 0);
    for (i = j = 0; j < size; j++)
	i = i * 256 + (unsigned char)key[j];
    return i;
}

void table_store(Table *tp, char *key, int size, Object obj,
	unsigned long flags) {
    int i = hash(key, size) % tp->size;
    Elem *p;

    for (p = tp->data[i]; p; p = p->next) {
	if (size == p->size && memcmp(key, p->data, size) == 0) break;
    }
    if (p) {
	deregister_object(p->obj);
    } else {
	p = safe_malloc(sizeof *p);
	p->data = safe_malloc(size);
	memcpy(p->data, key, size);
	p->size = size;
	p->next = tp->data[i];
	tp->data[i] = p;
    }
    p->obj = register_object(obj);
    p->flags = flags;
}

void table_remove(Table *tp, char *key, int size) {
    int i = hash(key, size) % tp->size;
    Elem *p, **pp;

    for (pp = &tp->data[i]; (p = *pp); pp = &p->next )
	if (size == p->size && memcmp(key, p->data, size) == 0) break;
    if (p) {
	*pp = p->next;
	deregister_object(p->obj);
	free(p->data);
	free(p);
    }
}

Elem *table_lookup(Table *tp, char *key, int size) {
    int i = hash(key, size) % tp->size;
    Elem *p;

    for (p = tp->data[i]; p; p = p->next) {
	if (size == p->size && memcmp(key, p->data, size) == 0) return p;
    }
    return 0;
}

/* $Revision: 1.5 $
 */

typedef struct _elem {
    char *data;
    int size;
    int obj;
    unsigned long flags;
    struct _elem *next;
} Elem;

typedef struct _table {
    Elem **data;
    int size;
} Table;

Table *table_new(int);
void table_delete(Table *);
void table_store(Table *, char *, int, Object, unsigned long);
void table_remove(Table *, char *, int);
Elem *table_lookup(Table *, char *, int);

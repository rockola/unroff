/* $Revision: 1.6 $
 */

/* The implementation of the Scheme primitive `file-insertions'.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "unroff.h"

static int cmp(const void *p1, const void *p2) {
    struct S_String *s1, *s2;
    Object o1, o2;
    int tmp;

    o1 = *(Object *)p1, s1 = STRING(Car(o1));
    o2 = *(Object *)p2, s2 = STRING(Car(o2));
    if ((tmp = memcmp(s1->data, s2->data,
	    s1->size < s2->size ? s1->size : s2->size)) == 0)
	if (s1->size == s2->size)
	    return Get_Integer(Car(Cdr(o1))) - Get_Integer(Car(Cdr(o2)));
	else
	    return s1->size - s2->size;
    return tmp;
}

#define Finish_File {\
    if (fromfn) {\
	copy_rest(from, to, fromfn, tofn);\
	close(from);\
	close(to);\
	if (rename(tofn, fromfn) == -1) {\
	    Saved_Errno = errno;\
	    Primitive_Error("cannot rename ~s to ~s: ~E", tofn, fromfn);\
	}\
	free(tofn);\
    }\
}

static void copy(int from, int to, char *fromfn, char *tofn,
	int len, int off) {
    char buf[8192];
    int n;

    while (len > 0) {
        if ((n = read(from, buf, len > 8192 ? 8192 : len)) == -1)
	    read_error(fromfn);
	if (n == 0)
	    fatal_error("file `%s' too short for insertion at offset %d",
		fromfn, off);
	if (write(to, buf, n) != n)
	    write_error(tofn);
	len -= n;
    }
}

static void copy_rest(int from, int to, char *fromfn, char *tofn) {
    char buf[8192];
    int n;

    while ((n = read(from, buf, 8192)) > 0)
	if (write(to, buf, n) != n)
	    write_error(tofn);
    if (n == -1)
	read_error(fromfn);
}

static Object p_file_insertions(Object spec) {
    Object v, x;
    int i, t, from = 0, to = 0;       /* make gcc -Wuninitialized happy */
    char *currfn, *fromfn, *tofn = 0; /* ditto */
    struct stat st;
    int off = 0, nextoff;             /* ditto for `off' */
    struct S_String *ins;

    v = P_List_To_Vector(spec);
    for (i = VECTOR(v)->size; --i >= 0; ) {
	x = VECTOR(v)->data[i];
	Check_List(x);
	if (Fast_Length(x) < 3 || TYPE(Car(x)) != T_String ||
		((t = TYPE(Car(Cdr(x)))) != T_Fixnum && t != T_Bignum) ||
		TYPE(Car(Cdr(Cdr(x)))) != T_String)
	    Primitive_Error("invalid file insertion specification");
    }
    qsort(VECTOR(v)->data, VECTOR(v)->size, sizeof(Object), cmp);
    for (fromfn = 0, i = 0; i < VECTOR(v)->size; i++) {
	x = VECTOR(v)->data[i];
	currfn = Get_String(Car(x));
	if (!fromfn || strcmp(fromfn, currfn) != 0) {
	    Finish_File;
	    fromfn = currfn;
	    if ((from = open(fromfn, O_RDONLY)) == -1)
		open_error(fromfn);
	    (void)fstat(from, &st);
	    if (st.st_nlink > 1) {
		warn("links to `%s' exist; insertion skipped", fromfn);
		close(from);
		fromfn = 0;
		continue;
	    }
	    tofn = safe_malloc(strlen(fromfn) + 1 + 4);
	    sprintf(tofn, "%s.new", fromfn);
	    if ((to = open(tofn, O_WRONLY|O_CREAT|O_TRUNC, st.st_mode)) == -1)
		open_error(tofn);
	    off = 0;
	}
	x = Cdr(x);
	nextoff = Get_Integer(Car(x));
	if (nextoff < 0)
	    Primitive_Error("invalid insertion offset ~a", Car(x));
	x = Cdr(x);
	ins = STRING(Car(x));
	copy(from, to, fromfn, tofn, nextoff-off, nextoff);
	if (write(to, ins->data, ins->size) != ins->size)
	    write_error(tofn);
	off = nextoff;
    }
    Finish_File;
    return Void;
}

void init_insert(void) {
    Define_Primitive(p_file_insertions, "file-insertions", 1, 1, EVAL);
}

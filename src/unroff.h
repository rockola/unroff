/* $Revision: 1.18 $
 */

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <time.h>
#include <math.h>


/* Include files that may be required by gcc although they shouldn't:
 */
#include <memory.h>


/* Prototypes that may be required by cc/gcc although they shouldn't:
 */
extern time_t time(time_t *);
extern size_t strftime(char *, size_t, const char *, const struct tm *);
extern long strtol(const char *s, char **endp, int base);
extern double strtod(const char *s, char **endp);
extern int pclose(FILE *);
extern int system(const char *);
#ifndef tolower
extern int tolower(int);
#endif

/* Prototypes for IEEE FP functions that may be missing:
 */
#ifndef finite
extern int finite(double);
#endif


#include "scheme.h"

/* Prototypes that were missing from "scheme.h" in some Elk releases:
 */
extern void Elk_Init(int ac, char **av, int call_inits, char *filename);
extern void Load_Source_Port(Object);


/* Used for passing a `char' to a function that takes an `int' argument,
 * such as isspace().  Maybe I should have used `unsigned char' rather
 * than `char' in the first place...
 */
#define UCHAR(c) ((unsigned char)(c))


#include "config.h"
#include "args.h"
#include "buffer.h"
#include "table.h"
#include "error.h"
#include "event.h"
#include "expr.h"
#include "gcroot.h"
#include "insert.h"
#include "malloc.h"
#include "parse.h"
#include "prim.h"
#include "scmtable.h"
#include "stream.h"
#include "subst.h"

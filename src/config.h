/* $Revision: 1.9 $
 */

/* Environment variable specifing library directory:
 */
#define DEFAULT_DIR_ENV        "UNROFF_DIR"


/* Environment variable specifing default output format:
 */
#define DEFAULT_FORMAT_ENV     "UNROFF_FORMAT"


/* User-supplied initialization file loaded from home directory:
 */
#define RC_FILE                ".unroff"


/* File to load if option -t is given:
 */
#define TEST_TOPLEVEL          "toplevel.scm"


#define MAJOR_VERSION          1
#define MINOR_VERSION          0


/* Hack: __GNUC_MINOR__ was introduced together with __attribute__ */
#ifdef __GNUC_MINOR__
#  define NORETURN __attribute__ ((noreturn))
#else
#  define NORETURN
#endif

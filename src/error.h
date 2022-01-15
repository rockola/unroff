/* $Revision: 1.7 $
 */

void set_progname(char *);
char *get_progname(void);

void warn(char *, ...);

void fatal_error(char *, ...) NORETURN;

void read_error(char *) NORETURN;
void write_error(char *) NORETURN;
void open_error(char *) NORETURN;

/* Redefined to add NORETURN
 */
extern Primitive_Error() NORETURN;

char *printable_string(char *, int);
char *printable_char(char);

/* $Revision: 1.5 $
 */

int istream_is_open(void);
int ostream_is_open(void);
char *curr_istream_target(void);
unsigned long curr_istream_lno(void);

int safe_readline(Buffer *);
void safe_write_char(char);
void safe_write(char *, int);

Object p_open_input_stream(Object);
Object p_close_stream(Object);
Object p_set_input_stream(Object);

void init_stream(void);

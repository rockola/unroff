/* $Revision: 1.8 $
 */

extern char escape;
extern char eqn_delim1, eqn_delim2;

int parse_expand(Buffer *, Buffer *);
int parse_escape(Buffer *, Buffer *, int, int);
void parse_line(Buffer *, Buffer *);
void parse_input(void);
void init_parse(void);

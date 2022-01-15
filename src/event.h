/* $Revision: 1.12 $
 */

/* Events:
 */
typedef enum _event {
    EV_LINE,          /* args: char|#f              ret: */
    EV_PROLOG,        /* args: pathname basename    ret: */
    EV_EPILOG,        /* args: pathname basename    ret: */
    EV_OPTION,        /* args: name value           ret: */
    EV_START,         /* args:                      ret: */
    EV_EXIT,          /* args:                      ret: */
    EV_REQUEST,       /* args: name args            ret: stringable */
    EV_MACRO,         /* args: name args            ret: stringable */
    EV_STRING,        /* args: name                 ret: stringable */
    EV_NUMREG,        /* args: name [sign]          ret: stringable */
    EV_SPECIAL,       /* args: name                 ret: stringable */
    EV_EQUATION,      /* args: string               ret: stringable */
    EV_SENTENCE,      /* args: char                 ret: stringable */
    EV_ESCAPE,        /* args: char [arg [sign]]    ret: stringable */
    EV_CHAR           /* args: char                 ret: stringable */
} Event;

/* Flags stored in `requests' table
 */
#define RQ_STRING  1
#define RQ_MACRO   2

void init_event(void);
Elem *event_lookup(Event, char *, int);
char *event_exec(Event, char *, int, int *, int);
char *event_exec_fallback(Event, char *, int, int *);
void events_vec_exec(Event);

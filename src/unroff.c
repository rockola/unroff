/* $Revision: 1.21 $
 */

#include "unroff.h"

extern int getopt(int ac, char * const *av, const char *optstring);

extern char *optarg;
extern int optind, opterr;

char *macros = "";
char *format;
char *directory;
int compatible;

static int got_filename;
static int did_ev_start;
static int tflag;

static void usage(void) {
    fprintf(stderr, "Usage: %s [options] [file...]\n\n", get_progname());
    fprintf(stderr, "  -mname      name of troff macro package (e.g. -ms)\n");
    fprintf(stderr, "  -fformat    output format (e.h. -fhtml2)\n");
    fprintf(stderr, "  -C          enable nroff/troff compatibility\n");
    fprintf(stderr, "  -hheapsize  Scheme heap size in Kbytes\n");
    fprintf(stderr, "  -t          enter interactive top-level for testing\n");
    fprintf(stderr, "  option=val  any macro-package/format-specific option\n");
}

static char *filename_tail(char *fn) {
    char *p = strrchr(fn, '/');

    return p && p > fn && p[1] ? p+1 : fn;
}

/* If the file name has the suffix .scm, load it into the interpreter.
 * Else, open an input stream and assign it to the current input stream.
 * Call start event procedure if necessary; trigger prolog event.
 * Start the parser; when finished, trigger epilog event.
 */
static void do_input_file(char *fn) {
    Object name, base, old, str;
    char *p;
    int len;
    GC_Node4;

    len = strlen(fn);
    if (len > 3 && strcmp(fn + len - 4, ".scm") == 0) {
	name = Make_String(fn, strlen(fn));
	(void)P_Load(1, &name);
	return;
    }
    if (fn[0] == '-' && fn[1] == 0)
	fn = "stdin";
    name = base = old = str = Null;
    GC_Link4(name, base, old, str);
    name = Make_String(fn, strlen(fn));
    p = filename_tail(fn);
    base = Make_String(p, strlen(p));
    str = p_open_input_stream(name);
    old = p_set_input_stream(str);
    if (!did_ev_start) {
	args_clear();
	events_vec_exec(EV_START);
	did_ev_start = 1;
    }
    args_clear();
    args_add(name);
    args_add(base);
    events_vec_exec(EV_PROLOG);
    parse_input();
    args_clear();
    args_add(name);
    args_add(base);
    events_vec_exec(EV_EPILOG);
    (void)p_set_input_stream(old);
    (void)p_close_stream(str);
    GC_Unlink;
}

/* Determine whether argument is a file name or an option; trigger
 * option event in the latter case.
 */
static void do_argument(char *arg) {
    char *p;

    if ((p = strchr(arg, '=')) == 0) {
	got_filename = 1;
	if (!tflag)
	    do_input_file(arg);
    } else {
	if (p == arg)
	    fatal_error("empty option name");
	args_clear();
	args_add(Make_String(arg, p++ - arg));
	args_add(Make_String(p, strlen(arg) - (p - arg)));
	events_vec_exec(EV_OPTION);
    }
}

/* Load scm/troff.scm.  The rest is loaded from there.
 */
static void boot_code(void) {
    char *fn = safe_malloc(strlen(directory) + 30);
    Object arg;

    sprintf(fn, "%s/scm/troff.scm", directory);
    arg = Make_String(fn, strlen(fn));
    (void)P_Load(1, &arg);
    free(fn);
}

/* Load $(HOME)/.RC_FILE, if it is there.
 */
static void load_rc_file(void) {
    FILE *f;
    char *home, *fn;
    Object port;
    GC_Node;

    if ((home = getenv("HOME")) == 0)
	return;
    fn = safe_malloc(strlen(home) + 30);
    sprintf(fn, "%s/%s", home, RC_FILE);
    if ((f = fopen(fn, "r")) != 0) {
	port = Make_Port(P_INPUT, f, Make_String(fn, strlen(fn)));
	GC_Link(port);
	Load_Source_Port(port);
	P_Close_Input_Port(port);    /* does the fclose() */
	GC_Unlink;
    }
    free(fn);
}

/* Load the interactive Scheme top-level.
 */
static void test_mode(void) {
    Object arg;
    char fn[] = TEST_TOPLEVEL;

    arg = Make_String(fn, strlen(fn));
    (void)P_Load(1, &arg);
}

int main(int ac, char **av) {
    char **eav;
    int eac = 1, c;

    if (ac == 0) {
	fprintf(stderr, "Oops--no argv[0]?\n"); return 1;
    }
    set_progname(av[0]);
    if ((directory = getenv(DEFAULT_DIR_ENV)) == 0)
	directory = DEFAULT_DIR;
    if ((format = getenv(DEFAULT_FORMAT_ENV)) == 0)
	format = DEFAULT_FORMAT;
    eav = safe_malloc((ac+1+2) * sizeof(char *));    /* ac + -p xxx + 0-ptr */
    eav[0] = av[0];
    opterr = 0;
    while ((c = getopt(ac, av, "h:gm:f:tC")) != EOF) {
	switch (c) {
	case 'g':
	    eav[eac++] = "-g"; break;
	case 'h':
	    if (strcmp(optarg, "elp") == 0) {
		usage();
		return 1;
	    }
	    eav[eac++] = "-h"; eav[eac++] = optarg; break;
	case 'm':
	    macros = optarg; break;
	case 'f':
	    format = optarg; break;
	case 't':
	    tflag = 1; break;
	case 'C':
	    compatible = 1; break;
	case '?':
	    usage();
	    return 1;
	}
    }
    /* Set the Elk load-path to $(directory)/elk, so that a minimal,
     * self-contained Elk runtime environment can be shipped with
     * binary distributions.  Sites with a full Elk can symlink
     * $(directory)/elk to the real directory.
     */
    eav[eac++] = "-p";
    eav[eac] = safe_malloc(strlen(directory) * 2 + 30);
    sprintf(eav[eac++], ".:%s/elk/scm:%s/elk/obj", directory, directory);
    eav[eac] = 0;
    Elk_Init(eac, eav, 0, 0);
    init_args();
    init_gcroot();
    init_insert();
    init_event();
    init_expr();
    init_parse();
    init_prim();
    init_scmtable();
    init_stream();
#ifdef ELK_MAJOR
    Set_Error_Tag("load");
#else
    Error_Tag = "load";
#endif
    boot_code();
    load_rc_file();
    if (tflag) {
	while (optind < ac)
	    do_argument(av[optind++]);
	if (got_filename)
	    warn("filename arguments are ignored when -t is given");
	test_mode();
	return 0;
    }
#ifdef ELK_MAJOR
    Set_Error_Tag("main-loop");
#else
    Error_Tag = "main-loop";
#endif
    while (optind < ac)
	do_argument(av[optind++]);
    if (!got_filename)
	do_input_file("-");
    if (did_ev_start) {
	args_clear();
	events_vec_exec(EV_EXIT);
    }
    return 0;
}

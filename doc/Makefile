# $Revision: 1.7 $

SHELL   = /bin/sh
UNROFF  = unroff

TROFF	= \
	unroff.1\
	unroff-html.1\
	unroff-html-man.1\
	unroff-html-ms.1\
	manual.ms

HTML    = \
	unroff.1.html\
	unroff-html.1.html\
        unroff-html-man.1.html\
	unroff-html-ms.1.html\
	manual.html


ALL	= $(HTML)

all:    $(ALL)

.SUFFIXES: .1 .1.html

.1.1.html:
	$(UNROFF) -man $<

manual.html:	manual.ms
	$(UNROFF) -ms document=manual hyper.scm $?

clean:
	rm -f $(ALL)

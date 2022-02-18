Installation Guide for the `unroff' Source Distribution
-------------------------------------------------------

Requirements: you need Elk 2.2 or Elk 3.0 (or later) and an ANSI C
compiler to install unroff from the source distribution.  If you don't
have Elk and don't want to install it, you may want to obtain the
binary unroff distribution instead.

You can obtain Elk at

   https://github.com/rockola/elk-scheme

Elk 3.0 used to be available at

   http://www.informatik.uni-bremen.de/~net/elk
   ftp://ftp.x.org/contrib/devel_tools/elk-3.0.tar.gz
   ftp://ftp.uni-bremen.de/pub/programming/languages/scheme/elk/elk-3.0.tar.gz



## Elk 2.2 patch
   If you are still using Elk 2.2, you may have to apply a small patch to
   your Elk installation.  The patch is in the file src/elk-2.2-patch.
   Assuming you have unpacked unroff under /usr/local/src/unroff-1.0,
   change to the directory where the Elk "src" directory is located and call:
   
      patch < /usr/local/src/unroff-1.0/src/elk-2.2-patch

   If the patch program detects a 'Reverse patch', the patch is not
   required and you are done.  If the patch succeeded, recompile the
   interpreter by calling `make` and `make install` in the directory "src".

## Makefile

o  Go to the sub-directory "src" below the directory where you unpacked
   unroff:

      cd /usr/local/src/unroff/src

   and edit the Makefile.  Check the site and compiler dependencies at the
   beginning of the Makefile and modify them if necessary.

   (Don't worry about DIR= if you just want to test unroff; you can
   override the directory later by setting the environment variable
   UNROFF_DIR before calling unroff.)

## Make

o  Call `make depend` and then `make`.

o  You may want to remove the minimal Elk runtime environment contained
   in the directory "elk" and replace it by a symbolic link to your
   site's Elk runtime directory (i.e. the directory with sub-directories
   "scm" and "obj").

## Test

o  Test unroff and the HTML back-end included in the distribution.

   For example, change to the directory "doc" and run "make" to convert
   the manual pages and the Programmer's Manual to HTML.  Then view the
   resulting .html files with your favorite WWW browser.

   Test the supplementary -ms features by calling (still in "doc"):

      unroff -ms document=test split=1 hyper.scm manual.ms

   This creates several files beginning with "test".  Load test.html
   into your WWW browser; observe the automatically generated table of
   contents and the hypertext links embedded in the document.  When
   finished, you may want to "rm test*".

   You can proceed by using unroff with a few troff documents of your
   own.  For example, try to convert a large manual page:

      unroff -man /usr/man/man1/csh.1

   Don't worry if this displays numerous warning messages.  unroff
   usually produces good results even if many low-level troff requests
   are ignored.  Check csh.1.html with your WWW browser.

o  Read the documentation located in the directory "doc", in particular
   the manual pages.
   
   You can convert the troff files to HTML and view them with your
   WWW browser (as explained in the previous step), or typeset them
   using your local troff and send the output to the printer, or read
   them online using nroff or man.

o  You may want to place a number of default settings into an initialization
   file ".unroff" in your home directory.  There is a sample init file
   "sample.unroff" in the directory "misc".  If you decide to use this
   file, replace the string "net@cs.tu-berlin.de" by your e-mail address.

o  If you find unroff useful, you may want to install it in a central
   directory at your site.  If so, install the executable, the contents
   of the directory "scm", and the manual pages.

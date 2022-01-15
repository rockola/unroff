;;;; -*-Scheme-*-
;;;;
;;;; $Revision: 1.20 $
;;;;
;;;; Common definitions for HTML output format


;;; --------------------------------------------------------------------------
;;; Configurable, site-specific definitions.

(define-option 'troff-to-gif  'string
  "groff -ms > %1%; /usr/www/lib/latex2html/pstogif %1% -out %2%")

(define-option 'troff-to-text 'string
   "groff -Tlatin1 -P-b -P-u |sed '/^[ \t]*$/d' > %1%")

(define-option 'tbl 'string 'gtbl)
(define-option 'eqn 'string 'geqn)
(define-option 'pic 'string 'gpic)


;; A non-breaking space that is really non-breaking even in broken browsers:

(define nbsp "&#160;<tt> </tt>")



;;; --------------------------------------------------------------------------
;;; Options.


(define-option 'title         'string  #f)    ; May be used for <title>
(define-option 'mail-address  'string  #f)    ; May be used for `mailto:'
(define-option 'document      'string  #f)    ; Prefix for output file(s)
(define-option 'tt-preformat  'boolean #f)    ; do <tt>-changes inside .nf/.fi

(define-option 'handle-eqn    'string "gif")  ; gif/text/copy
(define-option 'handle-tbl    'string "text") ;
(define-option 'handle-pic    'string "gif")  ;



;;; --------------------------------------------------------------------------
;;; Preformatted text.

(define preform? #f)

(define (preform on?)
  (cond ((and on? (not preform?))
          (defsentence #f)
          (with-font-preserved
	    (begin (set! preform? #t) "<pre>\n")))
        ((and (not on?) preform?)
          (defsentence sentence-event)
          (with-font-preserved
	    (begin (set! preform? #f) "</pre>\n")))
        (else "")))

(defrequest 'nf (lambda _ (preform #t)))
(defrequest 'fi (lambda _ (preform #f)))

(define-macro (with-preform-preserved . body)
  `(let (($p preform?))
     (concat (preform #f) ,@body (preform $p))))

(defchar #\tab
  (lambda (c)
    (if (not preform?) (surprise "tab outside .nf/.fi")) c))



;;; --------------------------------------------------------------------------
;;; Silently ignoring these requests probably will not harm.  There is
;;; nothing sensible we can do.

(defrequest 'ne "")
(defrequest 'hw "")
(defrequest 'nh "")
(defrequest 'hy "")
(defrequest 'lg "")
(defrequest 'ps "")
(defrequest 'vs "")
(defrequest 'pl "")
(defrequest 'bp "")
(defrequest 'ns "")
(defrequest 'rs "")
(defrequest 'wh "")
(defrequest 'ch "")
(defrequest 'fl "")
(defrequest 'na "")
(defrequest 'ad "")



;;; --------------------------------------------------------------------------
;;; Basic escape sequences and special characters.

(defescape #\c "")    ; swallows its character argument
(defescape #\& "")
(defescape #\- #\-)
(defescape #\| "")
(defescape #\^ "")
(defescape #\space #\space)    ; should be &#160; (doesn't work in Mosaic)
(defescape #\0 #\space)
(defescape #\s "")
(defescape #\e #\\)
(defescape #\\ #\\)
(defescape #\' #\')
(defescape #\` #\`)
(defescape #\% "")

(defescape ""
  (lambda (c . _)
    (warn "escape sequence `\\~a' expands to `~a'" c c)
    (translate c)))

(defspecial 'em "--")
(defspecial 'en #\-)
(defspecial 'mi #\-)
(defspecial 'pl #\+)        ; plus
(defspecial 'lq "``")
(defspecial 'rq "''")
(defspecial '** #\*)
(defspecial 'bv #\|)        ; bold vertical (what is this?)
(defspecial 'hy "&#173;")   ; `soft hyphen'
(defspecial 'co "&#169;")   ; copyright
(defspecial 'ap #\~)        ; approximates
(defspecial '~= #\~)
(defspecial 'cd "&#183;")   ; centered dot
(defspecial 'de "&#176;")   ; degree
(defspecial '>= "&gt;=")
(defspecial '<= "&lt;=")
(defspecial 'eq #\=)
(defspecial '== "==")
(defspecial 'mu "&#215;")   ; multiplication
(defspecial 'tm "&#174;")
(defspecial 'rg "&#174;")
(defspecial '*m "&#181;")   ; mu
(defspecial '*b "&#223;")   ; beta (#223 is German sharp-s actually)
(defspecial 'aa #\')        ; acute accent
(defspecial 'ga #\`)        ; grave accent
(defspecial 'br #\|)        ; vertical box rule
(defspecial 'or #\|)
(defspecial 'sl #\/)
(defspecial 'ru #\_)
(defspecial 'ul #\_)
(defspecial 'ci #\O)
(defspecial "14" "&#188;")
(defspecial "12" "&#189;")
(defspecial "34" "&#190;")
(defspecial 'es "&#216;")
(defspecial '+- "&#177;")
(defspecial 'sc "&#167;")
(defspecial 'fm #\')        ; foot mark
(defspecial 'lh "&lt;=")
(defspecial 'rh "=&gt;")
(defspecial '-> "-&gt;")
(defspecial '<- "&lt;-")
(defspecial 'no "&#172;")   ; negation
(defspecial 'di "&#247;")   ; division
(defspecial 'ss "&#223;")
(defspecial ':a "&#228;")
(defspecial 'a: "&#228;")
(defspecial ':o "&#246;")
(defspecial 'o: "&#246;")
(defspecial ':u "&#252;")
(defspecial 'u: "&#252;")
(defspecial ':A "&#196;")
(defspecial 'A: "&#196;")
(defspecial ':O "&#214;")
(defspecial 'O: "&#214;")
(defspecial ':U "&#220;")
(defspecial 'U: "&#220;")
(defspecial 'ct "&#162;")   ; cent
(defspecial 'Po "&#163;")   ; pound
(defspecial 'Cs "&#164;")   ; currency sign
(defspecial 'Ye "&#165;")   ; yen
(defspecial 'ff "ff")
(defspecial 'fi "fi")
(defspecial 'fl "fl")
(defspecial 'Fi "ffi")
(defspecial 'Fl "ffl")
(defspecial 'S1 "&#185;")
(defspecial 'S2 "&#178;")
(defspecial 'S3 "&#179;")
(defspecial 'bb "&#166;")   ; broken bar
(defspecial 'r! "&#161;")   ; reverse exclamation mark
(defspecial 'r? "&#191;")   ; reverse question mark


(defspecial 'bu (lambda _ (warn "rendering \\(bu as `+'") #\+))
(defspecial 'sq (lambda _ (warn "rendering \\(sq as `o'") #\o))
(defspecial 'dg (lambda _ (warn "rendering \\(dg as `**'") "**"))
(defspecial 'dd (lambda _ (warn "rendering \\(dd as `***'") "***"))



;;; --------------------------------------------------------------------------
;;; Local motion requests and related stuff (mostly ignored).

(define (motion-ignored request . _)
  (warn "local motion request \\~a ignored" request))

(defescape #\u motion-ignored)
(defescape #\d motion-ignored)
(defescape #\v motion-ignored)

(define (motion-no-effect request arg)
  (warn "local motion request \\~a has no effect" request)
  (parse arg))

(defescape #\o motion-no-effect)
(defescape #\z motion-no-effect)

(defescape #\k
  (lambda (k reg)
    ((requestdef 'nr) 'nr reg "0" "")))

(defescape #\h
  (lambda (h arg)
    (let* ((x (parse arg))
	   (n (get-hunits (parse-expression x 0 #\m))))
      (if (negative? n)
	  (warn "\\h with negative argument ignored")
	  (make-string n #\space)))))

(defescape #\w
  (lambda (w s)
    (let ((scale (get-scaling #\m))
	  (len (string-length (parse s))))
      (number->string (quotient (* len (car scale)) (cdr scale))))))

;; Heuristic: generate <hr> if length could be line length, else
;; repeat specified character:

(defescape #\l
  (lambda (l s)
    (let* ((p (parse-expression-rest s '(0 . "") #\m))
	   (n (get-hunits (car p)))
	   (c (parse (cdr p))))
      (if (>= n line-length)
	  "<hr>"
	  (repeat-string n (if (eqv? c "") "_" c))))))



;;; --------------------------------------------------------------------------
;;; Output translations for HTML special characters.

(defchar #\< "&lt;")
(defchar #\> "&gt;")
(defchar #\& "&amp;")

;;; Like parse, but also take char of `"':

(define (parse-unquote s)
  (let ((old (defchar #\" "&quot;")))
    (begin1 (parse s) (defchar #\" old))))



;;; --------------------------------------------------------------------------
;;; Font handling.

(define font-table (make-table 100))

(define (define-font name open close)
  (table-store! font-table name (cons open close)))

(define-font "R"  ""    "")
(define-font "I"  '<i>  '</i>)
(define-font "B"  '<b>  '</b>)
(define-font "C"  '<tt> '</tt>)
(define-font "CW" '<tt> '</tt>)
(define-font "CO" '<i>  '</i>)    ; a kludge for Courier-Oblique

(define font-positions (make-vector 10 #f))

(define (find-font f start)
  (cond
    ((= start (vector-length font-positions)) #f)
    ((equal? (vector-ref font-positions start) f) start)
    (else (find-font f (1+ start)))))

(define (font->position f)
  (let* ((m (find-font f 1)) (n (if m m (find-font #f 1))))
    (cond
      (n (mount-font n f) n)
      (else
	(warn "no free font position for font ~a" f) #f))))

(define (get-font-name name)
  (cond
    ((table-lookup font-table name) name)
    (else (warn "unknown font: ~a" name) "R")))

(define (mount-font i name)
  (if (and (>= i 1) (< i (vector-length font-positions)))
      (vector-set! font-positions i (get-font-name name))
      (warn "invalid font position: `~a'" i)))

(mount-font 1 "R")
(mount-font 2 "I")
(mount-font 3 "B")
(mount-font 4 "R")

(defrequest 'fp
  (lambda (fp where name)
    (if (not (string->number where))
	(warn "invalid font position `~a' in .fp" where)
	(mount-font (string->number where) name) "")))
	  
(define previous-font 1)
(define current-font  1)

(define (reset-font)
  (concat (change-font 1) (change-font 1)))    ; current and previous

(define (change-font-at i)
  (cond
    ((or (< i 1) (>= i (vector-length font-positions)))
      (warn "invalid font position: `~a'" i))
    ((vector-ref font-positions i)
      (let ((o (table-lookup font-table
			     (vector-ref font-positions current-font)))
	    (n (table-lookup font-table (vector-ref font-positions i))))
        (set! previous-font current-font)
        (set! current-font i)
	(if (and preform? (not (option 'tt-preformat)))
	    (concat (if (eq? (cdr o) '</tt>) "" (cdr o))
		    (if (eq? (car n) '<tt>)  "" (car n)))
            (concat (cdr o) (car n)))))
    (else (warn "no font mounted at position ~a" i))))

(define (change-font f)
  (cond
    ((number? f)
      (change-font-at f))
    ((string->number f)
      (change-font-at (string->number f)))
    ((string=? f "P")
      (change-font-at previous-font))
    (else
      (let ((n (font->position (get-font-name f))))
	(if n (change-font-at n) "")))))

(defrequest 'ft
  (lambda (ft font)
    (change-font (if (eqv? font "") "P" font))))

(defescape #\f (requestdef 'ft))

(defnumreg '.f (lambda _ (number->string current-font)))

(define-macro (with-font-preserved . body)
  `(let (($f current-font))
     (concat (change-font "R") ,@body (change-font $f))))



;;; --------------------------------------------------------------------------
;;; tbl, eqn, pic.

(define (copy-preprocess for-eqn? proc-1 proc-2 stop inline)
  (cond
    (inline
      (emit inline #\newline stop)
      (filter-eqn-line inline))
    (else
      (let loop ((x (read-line-expand))
	         (use-output? (not for-eqn?)))
           (cond ((eof-object? x) use-output?)
	         (else
	           (proc-1 (proc-2 x))
	           (if (string=? x stop)
		       use-output?
		       (loop (read-line-expand)
			     (or (not for-eqn?) (filter-eqn-line x))))))))))

(define troff-to-gif
  (let ((image-seqnum 1))
    (lambda (processor start stop what args inline)
      (let ((docname (option 'document)))
        (if (not docname)
	    (begin
	      (warn "~a skipped, because no `document' option given" what)
	      (if (not inline)
		  (skip-lines stop))
	      "")
            (let* ((num (number->string image-seqnum))
	           (psname (concat docname #\- num ".ps"))
	           (gifname (concat docname #\- num ".gif"))
		   (ref (concat "<img src=\"" gifname
				"\" alt=\"[" what "]\">\n"))
		   (use-output? #f))
	      (++ image-seqnum)
              (with-output-to-stream
	        (substitute (concat #\| (option processor)
				    #\| (option 'troff-to-gif)) psname gifname)
	        (emit start #\space (apply spread args) #\newline)
		(set! use-output? (copy-preprocess (eq? processor 'eqn)
				     emit identity stop inline)))
	      (remove-file psname)
	      (if use-output?
                  (if inline ref (concat "<p>" ref "<p>\n"))
		  (remove-file gifname) "")))))))

(define (troff-to-text processor start stop what args inline)
  (let* ((tmpname (substitute "%tmpname%"))
	 (use-output? #f))
    (with-output-to-stream
      (substitute (concat #\| (option processor) #\| (option 'troff-to-text))
		  tmpname)
      (emit start #\space (apply spread args) #\newline)
      (set! use-output? (copy-preprocess (eq? processor 'eqn)
			  emit identity stop inline)))
    (let ((text (translate (stream->string tmpname))))
      (remove-file tmpname)
      (if use-output?
	  (if inline
	      (with-font-preserved (concat (change-font 2) text))
	      (concat (preform #t) text (preform #f)))
	  ""))))

(define (troff-to-preform processor start stop what args inline)
  (cond
    (inline (with-font-preserved (concat (change-font 2) inline)))
    (else
      (emit (preform #t) start #\space (apply spread args) #\newline)
      (copy-preprocess (eq? processor 'eqn) emit translate stop)
      (preform #f))))

(define (troff-select-method option-name)
  (let ((method (option option-name)))
    (cond ((string=? method "gif")  troff-to-gif)
	  ((string=? method "text") troff-to-text)
	  ((string=? method "copy") troff-to-preform)
	  (else
	    (warn "bad value `~a' for ~a, assuming `text'" method option-name)
	    troff-to-text))))

(defmacro 'TS
  (lambda (TS . args)
    ((troff-select-method 'handle-tbl) 'tbl ".TS" ".TE\n" "table" args #f)))

(defmacro 'EQ
  (lambda (EQ . args)
    ((troff-select-method 'handle-eqn) 'eqn ".EQ" ".EN\n" "equation" args #f)))

(defmacro 'PS
  (lambda (PS . args)
    ((troff-select-method 'handle-pic) 'pic ".PS" ".PE\n" "picture" args #f)))

(defmacro 'TE "")
(defmacro 'EN "")
(defmacro 'PE "")

(defequation
  (lambda (eqn)
    ((troff-select-method 'handle-eqn) 'eqn ".EQ" ".EN\n" "equation" '() eqn)))



;;; --------------------------------------------------------------------------
;;; Miscellaneous troff requests.

(defrequest 'br
  (lambda _
    (if (positive? lines-to-center) "" "<br>\n")))

(defrequest 'sp
  (lambda (sp num)
    (let ((n (if (eqv? num "") 1 (get-vunits (parse-expression num 0 #\v)))))
      (cond
	((negative? n)
	  (warn ".sp with negative spacing ignored"))
	(preform?
	  (repeat-string n "\n"))
	((zero? n)
	  "<br>\n")
	(else
	  (with-font-preserved (repeat-string n "<p>\n")))))))

(defrequest 'ti
  (lambda (ti num)
    (let ((n (if (eqv? num "") 0 (get-hunits (parse-expression num 0 #\m)))))
      (if (negative? n)
	  (warn ".ti with negative indent ignored")
          (concat "<br>\n" (repeat-string n nbsp))))))


;;; There is no reasonable way to create markup for .tl; just emit the
;;; argument:

(defrequest 'tl
  (lambda (tl s)
    (let* ((p (parse s))
	   (t (parse-triple p)))
      (cond
	(t
	  (spread (car t) (cadr t) (cddr t) #\newline))
	((eqv? s "")
	   "")
	(else
	   (warn "badly formed .tl argument: `~a'" p))))))


;;; Until HTML can center, at least generate a <br> after each line:

(defrequest 'ce
  (lambda (ce num)
    (let ((n (if (eqv? num "") 1 (string->number num))))
      (if n
	  (center (round n))
	  (warn ".ce argument `~a' not understood" num)))))

(define lines-to-center 0)

(define (center n)
  (set! lines-to-center n)
  (defevent 'line 50 (if (positive? n) center-processor #f))
  "")

(define (center-processor c)
  (if (positive? (-- lines-to-center))
      (if (eqv? c #\newline)
          (emit "<br>\n")))
  (if (not (positive? lines-to-center))
      (center 0)))



;;; --------------------------------------------------------------------------
;;; Other definitions.

;;; Suppress comment if writing to a buffer, because in this case the
;;; output is likely to be re-read later (e.g. it may be a macro):

(defescape #\"
  (lambda (_ x)
    (let ((c (string-prune-right x "\n" x))
	  (old (defchar #\tab #f)))
      (if (and (not (eqv? c "")) (not (stream-buffer? (output-stream))))
          (emit "<!-- " (translate c) " -->\n"))
      (defchar #\tab old)
      #\newline)))


;;; Extra white space at end of sentence:

(define sentence-event
  (lambda (c)
    (concat c "<tt> </tt>\n")))

(defsentence sentence-event)


;;; Emit standardized output file prolog:

(define (emit-HTML-prolog)
  (let ((mailto (option 'mail-address)))
    (emit "<html>\n<head>\n")
    (emit "<!-- This file has been generated by "
	  (substitute "%progname% %version%, %date% %time%. -->\n")
	  "<!-- Do not edit! -->\n")
    (if mailto (emit "<link rev=\"made\" href=\"mailto:" mailto "\">\n"))))


;;; Define a scaling for the usual scaling indicators.  Note that the
;;; vertical spacing and character width will never change; and the
;;; device's vertical/horizontal resolution is 1.

(define inch 240)    ; units per inch

(set-scaling! #\i inch 1)
(set-scaling! #\c (* 50 inch) 127)
(set-scaling! #\P inch 6)    ; Pica
(set-scaling! #\m inch 10)
(set-scaling! #\n inch 10)
(set-scaling! #\p inch 72)
(set-scaling! #\v inch 7)

;;; Convert from units back to ems and Vs:

(define (get-hunits x)
  (let ((s (get-scaling #\m)))
    (if x (inexact->exact (/ (* x (cdr s)) (car s))) x)))

(define (get-vunits x)
  (let ((s (get-scaling #\v)))
    (if x (inexact->exact (/ (* x (cdr s)) (car s))) x)))

;;; Fake line length:

(define line-length 65)

(defnumreg '.l "1560")    ; 65 ems

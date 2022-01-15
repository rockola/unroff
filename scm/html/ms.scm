;;;; -*-Scheme-*-
;;;;
;;;; $Revision: 1.14 $
;;;;
;;;; `ms' specific definitions for HTML output format


;;; --------------------------------------------------------------------------
;;; Options.

(define-option 'signature          'string    "")
(define-option 'split              'integer   0)
(define-option 'toc                'boolean   #t)
(define-option 'toc-header         'string    "Table of Contents")
(define-option 'pp-indent          'integer   3)
(define-option 'footnotes-header   'string    "Footnotes")
(define-option 'footnote-reference 'string    "[note %1%]")
(define-option 'footnote-anchor    'string    "[%1%]")



;;; --------------------------------------------------------------------------
;;; Predefined strings and number registers.

(defstring 'Q  "``")
(defstring 'U  "''")
(defstring '-  "--")    ; em-dash
(defstring 'MO (substitute "%monthname+%"))
(defstring 'DY (substitute "%monthname+% %day%, %year%"))

(defnumreg 'PN #\0)



;;; --------------------------------------------------------------------------
;;; General bookkeeping.


(define split-sections? #f)    ; #t if `split' option is positive


(define-pair abstract   abstract?   ""               "<hr>\n")
(define-pair title      title?      "<h1>\n"         "</h1>\n")
(define-pair secthdr    secthdr?    "<h2>\n"         "</h2>\n")
(define-pair tag-para   tag-para?   "<dl>\n"         "</dl>\n")
(define-pair list-para  list-para?  "<ul>\n"         "</ul>\n")
(define-pair quoted     quoted?     "<blockquote>\n" "</blockquote>\n")

(define (reset-everything)
  (emit
    (reset-font)
    (center 0)
    (quoted #f)
    (secthdr #f)
    (preform #f)
    (tag-para #f)
    (list-para #f)
    (reset-title-features))
  (header-processor #f))

(define-nested-pair indent  indent-level  "<dl><dt><dd>\n" "</dl>\n")



;;; --------------------------------------------------------------------------
;;; Manage HTML output files.

(define HTML-streams '())

(define (push-HTML-stream file-suffix title-suffix)
  (let* ((docname (option 'document))
	 (title (option 'title))
	 (t (concat (if title title docname) title-suffix))
	 (fn (if file-suffix (concat docname file-suffix ".html") #f))
	 (s (if fn (open-output-stream fn) #f)))
    (close-stream (set-output-stream! #f))
    (set-output-stream! s)
    (list-push! HTML-streams fn)
    (emit-HTML-prolog)
    (emit "<title>" (translate t) "</title>\n</head><body>\n")))

(define (pop-HTML-stream)
  (if (not (eqv? (option 'signature) ""))
      (emit "<p><hr>\n" (substitute (option 'signature))) #\newline)
  (emit "</body>\n</html>\n")
  (list-pop! HTML-streams)
  (close-stream (set-output-stream! #f))
  (if (and (not (null? HTML-streams)) (car HTML-streams))
      (set-output-stream! (append-output-stream (car HTML-streams)))))



;;; --------------------------------------------------------------------------
;;; Callback procedure called by hyper.scm when creating hypertext anchor.

(define (query-anchor request label)
  (lambda (op)
    (case op
      (allowed?       #t)
      (emit-anchor?   #t)
      (filename
	(if (not (stream-file? (output-stream)))
	    (car HTML-streams)
	    (stream-target (output-stream)))))))



;;; --------------------------------------------------------------------------
;;; Generate hypertext reference and anchor.

(define (make-href type index contents)
  (let* ((docname (option 'document))
	 (file
	   (case type
	   ((section toc) (car HTML-streams))
	   (footnote (if split-sections? (concat docname "-notes.html") "")))))
    (format #f "<a href=\"~a#~a~a\">~a" file type index
	       (if contents (concat contents "</a>\n") ""))))

(define (make-anchor type index contents)
  (format #f "<a name=\"~a~a\">~a</a>" type index contents))



;;; --------------------------------------------------------------------------
;;; Automatically generated TOC.

(define auto-toc-entry
  (let ((last-level 0))
    (lambda (anchor entry level labelnum)
      (with-output-appended-to-stream "[autotoc]"
	(emit (repeat-string (- level last-level) "<ul>")
	      (repeat-string (- last-level level) "</ul>"))
	(set! last-level level)
	(if (positive? level)
	    (emit "<li>" (make-href 'section labelnum anchor) entry))))))

(define (auto-toc-spill)
  (auto-toc-entry "" "" 0 0)
  (let ((toc (stream->string "[autotoc]")))
    (if (not (eqv? toc ""))
	(emit "<h2>" (substitute (option 'toc-header)) "</h2>\n" toc))))



;;; --------------------------------------------------------------------------
;;; Start and exit event functions.

(defevent 'start 10
  (lambda _
    (set! split-sections? (positive? (option 'split)))
    (let ((docname (option 'document)))
      (if (not (or docname (option 'title)))
	  (quit "you must set either document= or title="))
      (if (and split-sections? (not docname))
	  (quit "you must set document= for non-zero `split'"))
      (push-HTML-stream (if docname "" #f) ""))))

(defevent 'exit 10
  (lambda _
    (reset-everything)
    (emit (indent 0))
    (footnote-processor 'spill)
    (do () ((null? (cdr HTML-streams))) (pop-HTML-stream))
    (if (option 'toc)
        (auto-toc-spill))
    (pop-HTML-stream)))



;;; --------------------------------------------------------------------------
;;; Title features, abstract.

(define got-title? #f)

(define (reset-title-features)
  (concat (title #f)
	  (begin1 (if got-title? "<hr>\n" "") (set! got-title? #f))))

(defmacro 'TL
  (lambda _
    (cond
      (got-title?
	(warn ".TL is only allowed once"))
      (else
        (reset-everything)
        (set! got-title? #t)
        (title #t)))))

(defmacro 'AU
  (lambda _
    (emit (title #f) "<p>\n" (change-font 2))
    (center 999)))

(defmacro 'AI
  (lambda _
    (emit (title #f) "<br>\n" (change-font 1))
    (center 999)))

(defmacro 'AB
  (lambda (AB . args)
    (reset-everything)
    (abstract #t)
    (cond ((null? args)
	    "<h2>ABSTRACT</h2>\n<p>\n")
          ((string=? (car args) "no")
	    "<p>\n")
	  (else
	    (concat "<h2>" (parse (car args)) "</h2>\n<p>\n")))))

(defmacro 'AE
  (lambda _
    (cond (abstract?  (reset-everything) (abstract #f))
	  (else       (warn ".AE without preceding .AB")))))



;;; --------------------------------------------------------------------------
;;; Numbered sections.

(define sections (list 0))

(define (increment-section! s n)
  (if (positive? n)
      (increment-section! (cdr s) (1- n))
      (set-car! s (if (char? (car s))
		      (integer->char (modulo (1+ (char->integer (car s))) 256))
		      (1+ (car s))))
      (set-cdr! s '())))

(define (section-number s n)
  (if (zero? n)
      ""
      (format #f "~a.~a" (car s) (section-number (cdr s) (1- n)))))

(define (verify-section-number s)
  (cond ((eqv? s "") #f)
	((string->number s) (string->number s))
	((char-alphabetic? (string-ref s 0)) (string-ref s 0))
	(else #f)))

(define (numbered-section args)
  (cond
    ((null? args)
      (increment-section! sections 0)
      (defstring 'SN (section-number sections 1))
      1)
    ((string=? (car args) "S")
      (cond
	((null? (cdr args))
	  (warn ".NH with `S' argument but no numbers")
	  1)
	(else
	  (let ((new (map verify-section-number (cdr args))))
	    (if (memq #f new)
		(warn "bad section number in .NH request")
		(set! sections new))
	    (defstring 'SN (section-number new (length new)))
            (length new)))))
    (else
      (let ((level (string->number (car args))))
	(if (not level)
	    (begin
	      (warn "~a is not a valid section level" (car args))
	      (set! level 1)))
	(if (< (length sections) level)
	    (append! sections (make-list (- level (length sections)) 0)))
	(increment-section! sections (1- level))
	(defstring 'SN (section-number sections level))
	level))))

(defmacro 'NH
  (lambda (NH . args)
    (reset-everything)
    (emit (indent 0))
    (let ((level (numbered-section args)))
      (if (and split-sections? (<= level (option 'split)))
	  (let* ((sect (stringdef 'SN))
	         (suff (concat #\- (string-prune-right sect "." sect))))
	    (push-HTML-stream suff (concat ", section " sect))))
      (header-processor #t level))))

(define header-processor
  (let ((stream #f) (inside? #f) (seq 1) (level 0))
    (lambda (enter? . arg)
      (cond
	((and enter? (not inside?))
	  (set! level (car arg))
	  (set! stream (set-output-stream! (open-output-stream "[header]"))))
	((and inside? (not enter?))
	  (close-stream (set-output-stream! stream))
	  (let ((hdr (stream->string "[header]"))
		(sectno (stringdef 'SN)))
	    (cond
	      ((and split-sections? (option 'toc))
	        (auto-toc-entry (concat sectno #\space) hdr level seq)
	        (emit "<h2>" (make-anchor 'section seq sectno)))
	      (else
		(emit "<h2>" sectno)))
	    (emit nbsp hdr "</h2>\n")
	    (++ seq))))
      (set! inside? enter?)
      "")))



;;; --------------------------------------------------------------------------
;;; Font switching and related requests.

(define (with-font font . args)
  (let ((old current-font))
  (cond
    ((null? args)
       (concat (change-font font) #\newline))
    ((null? (cdr args))
       (concat (change-font font) (parse (car args) #\newline)
	       (change-font old)))
    (else
       (concat (change-font font) (parse (car args)) (change-font old)
	       (parse (cadr args) #\newline))))))

(defmacro 'I with-font)
(defmacro 'B with-font)
(defmacro 'R with-font)

(defmacro 'UL (lambda (UL) (with-font "I")))    ; <u> doesn't work

(defmacro 'SM
  (lambda (SM . words)
    (if (null? words) "" (parse (apply spread words) #\newline))))

(defmacro 'LG (requestdef 'SM))



;;; --------------------------------------------------------------------------
;;; Indented paragraph with optional label.

(define (indented-paragraph IP . arg)
  (define (non-tagged? s)
    (or (null? s) (member (car s) '("\\(bu" "\\(sq" "\\-"))))
  (emit (reset-font) (secthdr #f) (reset-title-features))
  (header-processor #f)
  (cond
    (preform?
      (surprise ".IP inside .nf/.fi")
      (if (not (null? arg)) (concat (parse (car arg)) #\newline) #\newline))
    (tag-para?
      (if (null? arg)
	  "<dt><dd><p>\n"
	  (concat "<dt>" (parse (car arg)) "<dd>\n")))
    (list-para?
      (cond
	((non-tagged? arg)
	  "<li>\n")
	(else
          (warn ".IP `arg' in a list that was begun as non-tagged")
          (concat "<li>" (parse (car arg)) "<br>\n"))))
    ((non-tagged? arg)
      (concat (list-para #t) (indented-paragraph IP)))
    (else
      (concat (tag-para #t) (indented-paragraph IP (car arg))))))

(defmacro 'IP indented-paragraph)



;;; --------------------------------------------------------------------------
;;; Relative indent.

(define (relative-indent request . _)
  (if preform?
      (surprise ".RS/.RE inside .nf/.fi"))
  (emit (reset-font) (tag-para #f) (list-para #f))
  (with-preform-preserved
    (indent (if (string=? request "RS") '+ '-))))

(defmacro 'RS relative-indent)
(defmacro 'RE relative-indent)



;;; --------------------------------------------------------------------------
;;; Displays.

(define display-saved-font #f)
(define inside-display? #f)
(define indented-display? #f)

(define (display-start type)
  (if (or (string=? type "C") (string=? type "B"))
      (begin
	(warn "display type ~a not supported (using I)" type)
	(set! type "I")))
  (cond
    ((or (not (= (string-length type) 1))
	 (not (memq (string-ref type 0) '(#\I #\L #\C #\B))))
      (warn "illegal display type `~a'" type))
    (inside-display?
      (warn "nested display ignored"))
    (preform?
      (warn "display inside .nf/.fi ignored"))
    (else
      (set! display-saved-font current-font)
      (emit (reset-font))
      (set! indented-display? (string=? type "I"))
      (if indented-display?
          (emit (indent '+)))
      (set! inside-display? #t)
      (preform #t))))

(defmacro 'DS
  (lambda (DS . args)
    (display-start (if (null? args) "I" (car args)))))

(defmacro 'ID (lambda _ (display-start "I")))
(defmacro 'LD (lambda _ (display-start "L")))
(defmacro 'CD (lambda _ (display-start "C")))
(defmacro 'BD (lambda _ (display-start "B")))

(defmacro 'DE
  (lambda _
    (cond
      ((not inside-display?)
	(warn ".DE without matching display start"))
      (else
	(set! inside-display? #f)
	(emit
	  (with-font-preserved
	    (preform #f)
	    (if indented-display? (indent '-) ""))
	  (change-font display-saved-font))
	""))))



;;; --------------------------------------------------------------------------
;;; Footnotes.

;; Generating \[***] for \** allows us to defer creating the anchor from
;; string expansion time to output time.  Otherwise we couldn't use <...>.

(defstring '* "\\[***]")

(define **-count 0)

(defspecial '***
  (lambda _
    (++ **-count)
    (footnote-anchor (substitute (option 'footnote-reference)
				 (number->string **-count)))))

(define next-footnote 0)

(define (footnote-anchor sym)
  (++ next-footnote)
  (with-font-preserved
    (concat (change-font 1) (make-href 'footnote next-footnote sym))))

;; New request to generate a footnote anchor; an alternative to \**.
;; Should be followed by .FS.  Do not use `.FA \**'.

(defmacro 'FA
  (lambda (FA arg) (footnote-anchor (parse arg))))


(define footnote-processor
  (let ((stream #f) (inside? #f))
    (lambda (op . arg)
      (case op
      (begin
        (cond
	  (inside?
	    (surprise "nested .FS"))
	  (else
	    (set! inside? #t)
	    (set! stream (set-output-stream!
			   (append-output-stream "[footnotes]")))
	    (emit "<p>\n")
	    (let ((anchor
		    (cond ((not (null? arg))
			    (parse (car arg)))
			  ((positive? **-count)
			    (substitute (option 'footnote-anchor)
					(number->string **-count)))
			  (else #f))))
	      (if anchor
		  (emit "<b>" (make-anchor 'footnote next-footnote anchor)
			"</b>" nbsp))))))
      (end
	(cond
	  (inside?
	    (set! inside? #f)
	    (close-stream (set-output-stream! stream)))
	  (else (warn ".FE without matching .FS"))))
      (spill
	(if inside? (quit "unterminated footnote at end of document"))
	(let ((contents (stream->string "[footnotes]"))
	      (hdr (substitute (option 'footnotes-header))))
	  (cond
	    ((not (eqv? contents ""))
	       (if split-sections?
		   (push-HTML-stream "-notes" ", footnotes"))
	       (cond ((and split-sections? (option 'toc))
		       (auto-toc-entry hdr "" 1 0)
		       (emit "<h2>" (make-anchor 'section 0 hdr)))
		     (else (emit "<h2>" hdr)))
	       (emit "</h2>\n" contents))
	    ((positive? next-footnote)
	      (warn "footnote anchor used, but no .FS"))))))
    "")))

(defmacro 'FS
  (lambda (FS . arg)
    (apply footnote-processor 'begin arg)))

(defmacro 'FE
  (lambda _ (footnote-processor 'end)))



;;; --------------------------------------------------------------------------
;;; TOC macros.

(define toc-processor
  (let ((stream #f) (inside? #f) (seq 1))
    (lambda (op . arg)
      (case op
      (begin
        (cond
	  (inside?
	    (surprise "nested .XS"))
	  (else
	    (set! inside? #t)
	    (emit (make-anchor 'toc seq "&#160;") #\newline)
	    (set! stream (set-output-stream! (append-output-stream "[toc]")))
	    (if (>= (length arg) 2)
		(emit
		  (repeat-string
		    (get-hunits (parse-expression (cadr arg) 0 #\n)) nbsp)))
	    (if (option 'document)
	        (emit (make-href 'toc seq #f)))
	    (++ seq))))
      (end
	(cond
	  (inside?
	    (set! inside? #f)
	    (if (option 'document) (emit "</a>\n"))
	    (emit "<br>\n")
	    (close-stream (set-output-stream! stream)))
	  (else (warn ".XE or .XA without matching .XS"))))
      (spill
	(if inside? (quit "unterminated .XE"))
	(if (or (null? arg) (not (string=? (car arg) "no")))
	    (emit "<h2>Table of Contents</h2>\n"))
	(emit (stream->string "[toc]"))))
    "")))

(defmacro 'XS
  (lambda (XS . arg)
    (apply toc-processor 'begin arg)))

(defmacro 'XE (lambda _ (toc-processor 'end)))
(defmacro 'XA (lambda _ (toc-processor 'end) (toc-processor 'begin)))

(defmacro 'PX
  (lambda (PX . arg)
    (apply toc-processor 'spill arg)))


;;; --------------------------------------------------------------------------
;;; Paragraphs of various kinds.

(define-macro (define-paragraph request . body)
  `(defmacro ,request (lambda _ (reset-everything) ,@body)))

(define-paragraph 'LP "<p>\n")
(define-paragraph 'PP (concat "<p>\n"
			      (repeat-string (option 'pp-indent) nbsp)))
(define-paragraph 'QP (quoted #t))
(define-paragraph 'SH (secthdr #t))
(define-paragraph 'RT)



;;; --------------------------------------------------------------------------
;;; Requests that must be ignored, either because the function cannot
;;; be expressed in HTML or because they assume a page structure.

(defmacro 'AM "")    ; better accents
(defmacro 'BT "")    ; bottom title
(defmacro 'CM "")    ; cut mark between pages
(defmacro 'CT "")    ; chapter title
(defmacro 'DA "")    ; force date at page bottom
(defmacro 'EF "")    ; even footer
(defmacro 'EH "")    ; even header
(defmacro 'HD "")    ; optional page header
(defmacro 'KE "")    ; keep end
(defmacro 'KF "")    ; floating keep
(defmacro 'KS "")    ; keep
(defmacro 'ND "")    ; no date in footer
(defmacro 'NL "")    ; reset point size to normal
(defmacro 'OF "")    ; odd footer
(defmacro 'OH "")    ; odd header
(defmacro 'P1 "")    ; print header on 1st page
(defmacro 'PT "")    ; page title
(defmacro 'TM "")    ; UCB thesis mode

(defmacro 'BX        ; boxed word
  (lambda (BX word)
    (parse word #\newline)))

(define (multi-column-ignored request . _)
  (warn "multi-column request .~a not supported" request))

(defmacro 'MC multi-column-ignored)
(defmacro '1C multi-column-ignored)
(defmacro '2C multi-column-ignored)



;;; --------------------------------------------------------------------------
;;; Anachronisms, kludges, etc.

(defmacro 'UX "UNIX")

(defmacro 'B1 "<hr>\n")
(defmacro 'B2 "<hr>\n")

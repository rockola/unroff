;;;; -*-Scheme-*-
;;;;
;;;; $Revision: 1.18 $
;;;;
;;;; `man' specific definitions for HTML output format


;;; --------------------------------------------------------------------------
;;; Options.

(define-option 'do-signature 'boolean #t)



;;; --------------------------------------------------------------------------
;;; Miscellaneous definitions.

(defstring 'R "&#174;")        ; trademark
(defstring 'S "")              ; change to default point size
(defstring 'lq "``")
(defstring 'rq "''")

(define-font 'L '<b> '</b>)    ; whatever font L is supposed to be...



;;; --------------------------------------------------------------------------
;;; Bookkeeping for .TH, for requests that occur in pairs, etc.

(define-pair header     header?     "<h2>\n"    "<hr></h2>\n")
(define-pair tag-para   tag-para?   "<dl>\n"    "</dl>\n")
(define-pair list-para  list-para?  "<ul>\n"    "</ul>\n")
(define-pair hang-para  hang-para?  "<dt>"      "<dd>\n")

(define (reset-everything)
  (concat
    (reset-font)
    (center 0)
    (header #f)
    (preform #f)
    (hang-para #f)
    (tag-para #f)
    (list-para #f)))

(define-nested-pair indent  indent-level  "<dl><dt><dd>\n" "</dl>\n")



;;; --------------------------------------------------------------------------
;;; File prolog and epilog functions.

(defevent 'prolog 10
  (lambda (pathname filename)
    (if (not (string=? filename "stdin"))
	(set-option! 'document filename))
    (let ((docname (option 'document)))
      (if docname
	  (set-output-stream! (open-output-stream (concat docname ".html"))))
      (emit-HTML-prolog))))

(defevent 'epilog 10
  (lambda _
    (complain-if-no-title)
    (emit (reset-everything) (indent 0))
    (if (option 'do-signature)
	(emit
          (substitute
	    "<p><hr>\nMarkup created by <em>%progname%</em> %version%,")
	  nbsp nbsp
          (substitute "%monthname+% %day%, %year%.\n")))
    (emit "</body>\n</html>\n")
    (close-stream (set-output-stream! #f))))



;;; --------------------------------------------------------------------------
;;; Title, section, subsection.

(define title-seen? #f)

(define (complain-if-no-title)
  (if (not title-seen?)
      (quit "manual page must begin with .TH request")))

(defmacro 'TH
  (lambda (TH what section . _)
    (let ((title (option 'title)))
      (set! title-seen? #t)
      (concat "<title>"
	      (substitute (if title title "Manual page for %1%(%2%)")
			  (translate what) (translate section))
	      "</title>\n</head>\n<body>\n"))))

(defmacro 'SH
  (lambda (SH first . rest)
    (complain-if-no-title)
    (emit (reset-everything) (indent 0))
    (if (string=? first "NAME")
	(header #t)
	(concat "<h2>" (parse (apply spread first rest)) "</h2>\n"))))

(defmacro 'SS
  (lambda (SS . args)
    (complain-if-no-title)
    (emit (reset-everything) (indent 0))
    (cond
      ((null? args)
	 (defevent 'line 11
	   (lambda _ (emit "</h3>\n") (defevent 'line 11 #f)))
	 (emit "<h3>"))
      (else
	(concat "<h3>" (parse (apply spread args)) "</h3>\n")))))



;;; --------------------------------------------------------------------------
;;; Font switching requests.
;;;
;;; Both with-font and with-fonts include a terminating newline in the
;;; parsing, because people are using  .I xxx\c and .BR xxx\c  etc., and
;;; end-of-sentence must be detected in situations like .BR send(2) .

(define (with-font font words)
  (let ((old current-font))
    (cond
      ((null? words)
	 (defevent 'line 10
	   (lambda _ (emit (change-font old)) (defevent 'line 10 #f)))
	 (emit (change-font font) #\newline))
      (else
        (concat (change-font font)
		(parse (apply spread words) #\newline)
		(change-font old))))))

(defmacro 'I (lambda (I . args) (with-font "I" args)))
(defmacro 'B (lambda (B . args) (with-font "B" args)))

(defmacro 'SB (requestdef 'B))

(defmacro 'SM
  (lambda (SM . words)
    (if (null? words) "" (parse (apply spread words) #\newline))))

(define (with-fonts f1 f2 words)
  (define (recurse f1 f2 words)
    (if (null? words)
         ""
         (concat (change-font f1)
		 (parse (concat (car words)
				(if (null? (cdr words)) #\newline "")))
		 (recurse f2 f1 (cdr words)))))
  (let ((old current-font))
    (concat (recurse f1 f2 words) (change-font old))))

(defmacro 'BI (lambda (BI . args) (with-fonts "B" "I" args)))
(defmacro 'BR (lambda (BR . args) (with-fonts "B" "R" args)))
(defmacro 'IB (lambda (IB . args) (with-fonts "I" "B" args)))
(defmacro 'IR (lambda (IR . args) (with-fonts "I" "R" args)))
(defmacro 'RB (lambda (RB . args) (with-fonts "R" "B" args)))
(defmacro 'RI (lambda (RB . args) (with-fonts "R" "I" args)))



;;; --------------------------------------------------------------------------
;;; Indented paragraphs with labels.
;;;
;;; A heuristic is used to determine whether to emit a bulleted list
;;; or a tagged list:  .TP with \(bu in the next input line and
;;; .IP with \(bu as argument both start a bulleted list.  Of course, in
;;; case the style changes later, we have a problem and may want to end
;;; the current list and begin a new one with the new style.

(define (next-para-TP)
  (cond
    (tag-para?
      (defevent 'line 12
	(lambda _ (emit (reset-font) "<dd>\n") (defevent 'line 12 #f)))
      (emit "<dt>"))
    (else
      "<li>")))

(define (next-para-IP arg)
  (cond
    (tag-para?
      (if (null? arg)
	  "<dt><dd><p>\n"
	  (concat "<dt>" (parse (car arg)) "<dd>\n")))
    ((or (null? arg) (string=? (car arg) "\\(bu"))
      "<li>\n")
    (else
      (warn ".IP `arg' in a list that was begun as non-tagged")
      (concat "<li>" (parse (car arg)) "<br>\n"))))

(defmacro 'TP
  (lambda _
    (emit (reset-font) (hang-para #f))
    (if preform?
	(begin
	  (surprise ".TP inside .nf/.fi") #\newline)
        (let ((next (read-line)))
	  (if (eof-object? next) (set! next #\newline))
          (cond
	    ((string=? next "\\(bu\n")
	      (cond
	        (tag-para?
	          (emit (tag-para #f) (list-para #t)))    ; change style
	        (else
	          (emit (list-para #t)))))
	    (else
	      (unread-line next)
	      (cond
	        (list-para?
	          (emit (list-para #f) (tag-para #t)))    ; change style
                (else
	          (emit (tag-para #t))))))
        (next-para-TP)))))

(defmacro 'IP
  (lambda (IP . arg)
    (emit (reset-font) (hang-para #f))
    (if preform?
	(begin
	  (surprise ".IP inside .nf/.fi")
	  (if (not (null? arg)) (concat (parse (car arg)) #\newline) #\newline))
        (if (or tag-para? list-para?)
	    (next-para-IP arg)
	    (cond
	      ((and (not (null? arg)) (string=? (car arg) "\\(bu"))
	        (emit (list-para #t))
	        (set! arg '()))
	      (else
	        (emit (tag-para #t))))
	    (next-para-IP arg)))))


;;; A hanging indent cannot be achieved in HTML.  Therefore we have to
;;; kludge .HP by beginning a `tag-para' and putting everything up to
;;; the next line break between the <dt> and <dd>.

(defmacro 'HP
  (lambda _
    (emit (reset-font) (hang-para #f))
    (cond
      (preform?
	(surprise ".HP inside .nf/.fi") #\newline)
      (else
	(if list-para? (emit (list-para #f)))    ; change style
	(concat (tag-para #t) (hang-para #t))))))



;;; --------------------------------------------------------------------------
;;; Relative indent.

(define (relative-indent request . _)
  (if preform?
      (surprise ".RS/.RE inside .nf/.fi"))
  (emit (reset-font) (hang-para #f) (tag-para #f) (list-para #f))
  (with-preform-preserved
    (indent (if (string=? request "RS") '+ '-))))

(defmacro 'RS relative-indent)
(defmacro 'RE relative-indent)



;;; --------------------------------------------------------------------------
;;; Paragraphs.

(define (paragraph . _)
  (concat (reset-everything) "<p>\n"))

(defmacro 'LP paragraph)
(defmacro 'PP paragraph)
(defmacro 'P  paragraph)



;;; --------------------------------------------------------------------------
;;; Miscellaneous break-causing requests (must end .HP paragraph).

(defrequest 'sp
  (let ((orig (requestdef 'sp)))
    (lambda (sp num)
      (concat (hang-para #f) (orig sp num)))))

(defrequest 'bp (lambda _ (hang-para #f)))

(defrequest 'ti
  (let ((orig (requestdef 'ti)))
    (lambda (ti num)
      (concat (hang-para #f) (orig ti num)))))

;;; Kludge: Suppress <br> immediately after `hang-para' to avoid excessive
;;; white space

(defrequest 'br
  (lambda _
    (if hang-para?
	(hang-para #f)
        (concat (hang-para #f) "<br>\n"))))



;;; --------------------------------------------------------------------------
;;; Bogus or SunOS-specific stuff.

(defmacro 'TX
  (lambda (TX name . id)
    (concat "[a manual with the abbreviation " (parse name) "]"
	    (if (null? id) "" (car id)) #\newline)))

(defmacro 'IX "")
(defmacro 'DT "")
(defmacro 'PD "")
(defmacro 'UC "")

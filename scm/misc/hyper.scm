;;;; -*-Scheme-*-
;;;;
;;;; $Revision: 1.5 $
;;;;
;;;; General-purpose hypertext requests
;;;;
;;;; This implementation is undocumented and is likely to change in
;;;; future relases.


(if (not (string=? (substitute "%format%") "html"))
    (quit "hypertext functions require -fhtml"))



;;; --------------------------------------------------------------------------
;;; Data structures.

(define ht-anchors '())
(define ht-references '())


(define anchor-name car)
(define anchor-location cdr)

(define (anchor-create name location)
  (cons name location))

(define ref-filename car)
(define ref-offset cadr)
(define ref-location caddr)
(define ref-name cadddr)

(define (ref-unresolved? x)
  (eqv? (ref-location x) ""))

(define (set-ref-location! x l)
  (set-car! (cddr x) l))

(define (ref-create filename offset name)
  (list filename offset "" name))



;;; --------------------------------------------------------------------------
;;; Figure out if hypertext request is allowed at a given place and the
;;; filename to use in hypertext anchor.

(define (default-query-anchor request label)
  (lambda (op)
    (case op
      (allowed?       #t)
      (emit-anchor?   #t)
      (filename       (stream-target (output-stream))))))

(define (ht-querier request label)
  (let ((q (if (bound? 'query-anchor) query-anchor default-query-anchor)))
    (q request label)))



;;; --------------------------------------------------------------------------
;;; Create hypertext anchor.
;;;
;;; .Ha label anchor-text

(defmacro 'Ha
  (lambda (Ha name contents)
    (let* ((q (ht-querier '.Ha name))
	   (location (q 'filename)))
      (cond
	((not (q 'allowed?))
	  "")
	((assoc name ht-anchors)
	  (warn ".Ha with duplicate anchor name `~a'" name))
	(else
	  (resolve-ht-reference name location)
          (list-push! ht-anchors (anchor-create name location))
	  (if (q 'emit-anchor?)
	      (concat (format #f "<a name=\"~a\">~a</a>" (parse-unquote name)
		      (parse contents)))
	      ""))))))

(define (resolve-ht-reference name location)
  (let loop ((x ht-references))
       (cond
	 ((not (null? x))
           (if (and (ref-unresolved? (car x))
		    (string=? (ref-name (car x)) name))
               (set-ref-location! (car x) location))
           (loop (cdr x))))))



;;; --------------------------------------------------------------------------
;;; Create hypertext reference.
;;;
;;; .Hr -url      url   anchor-text [suffix]
;;; .Hr -symbolic label anchor-text [suffix]
;;; .Hr troff-text

(defmacro 'Hr
  (lambda (Hr type . args)
    (cond
      ((string=? type "-url")
	(if (< (length args) 2)
	    (warn "too few arguments for .Hr")
	    (concat
	      (format #f "<a href=\"~a\">~a</a>~a"
		      (parse-unquote (car args))
		      (parse (cadr args))
		      (if (null? (cddr args))
			  #\newline
			  (parse (caddr args) #\newline))))))
      ((string=? type "-symbolic")
	(if (< (length args) 2)
	    (warn "too few arguments for .Hr")
            (let* ((ref (car args))
		   (q (ht-querier '.Hr ref))
	           (filename (q 'filename))
	           (a (assoc ref ht-anchors))
	           (location (if a (cdr a) "")))
              (cond
		((not (q 'allowed?))
		  "")
	        ((and (not a) (not (output-stream)))
	           (warn ".Hr forward reference requires `document' option"))
	        (else
	          (emit "<a href=\"")
	          (if (not a)
	              (list-push! ht-references
		        (ref-create filename (stream-position (output-stream))
				    ref)))
	          (concat (if (string=? filename location) "" location)
	                  #\# (parse-unquote ref) "\">"
		          (parse (cadr args)) "</a>"
			  (if (null? (cddr args))
			      #\newline
			      (parse (caddr args) #\newline))))))))
      (else ""))))



;;; --------------------------------------------------------------------------
;;; Complain about unresolved references and weed out references that
;;; point into the file where they appear; then do the file insertions.

(define (check-ht-references)
  (let loop ((x ht-references) (new '()))
       (cond
	 ((null? x)
	   new)
	 ((ref-unresolved? (car x))
	   (warn "unresolved hypertext reference `~a' in file ~a"
		 (ref-name (car x)) (ref-filename (car x)))
	   (loop (cdr x) new))
	 ((string=? (ref-filename (car x)) (ref-location (car x)))
	   (loop (cdr x) new))
	 (else
	   (loop (cdr x) (cons (car x) new))))))

(define (insert-ht-references)
  (let ((refs (check-ht-references)))
    (file-insertions refs)))

(defevent 'exit 90 insert-ht-references)

;;;; -*-Scheme-*-
;;;;
;;;; $Revision: 1.22 $
;;;;
;;;; Basic initializations


;;; --------------------------------------------------------------------------
;;; Define minimal reset, interrupt handler, and error handlers.

(if (call-with-current-continuation
      (lambda (c)
	(set! top-level-control-point c) #f))
    (exit 1))

(define (interrupt-handler) (exit 1))

(define (error-handler . args)
  (let ((port (error-port)))
    (format port "~a: ~s: " (substitute "%progname%") (car args))
    (apply format port (cdr args))
    (newline port)
    (exit 1)))



;;; --------------------------------------------------------------------------
;;; Procedures to print an error message and quit and to print warnings.

(define (quit msg . args)
  (let ((port (error-port)))
    (display (substitute "%progname%:%filepos% ") port)
    (apply format port msg args)
    (newline port))
  (exit 1))

(define (warn msg . args)
  (let ((port (error-port)))
    (display (substitute "%progname%:%filepos% warning: ") port)
    (apply format port msg args)
    (newline port)
    ""))    ; return "" to assist use in event functions

(define (surprise msg)
  (warn (concat msg " may not work as expected")))



;;; --------------------------------------------------------------------------
;;; Miscellaneous utilities.

(define-macro (++ var) `(set! ,var (1+ ,var)))
(define-macro (-- var) `(set! ,var (1- ,var)))

(define (identity x) x)


(define (copy-apply reader . procedures)
  (define (apply-all val procs)
    (if (null? procs)
	val
	((car procs) (apply-all val (cdr procs)))))
  (let loop ((x (reader)))
       (cond ((eof-object? x) "")
	     (else
	       (apply-all x procedures)
	       (loop (reader))))))


(define-macro (list-push! list elem)
  `(set! ,list (cons ,elem ,list)))

(define-macro (list-pop! list)
  `(set! ,list (cdr ,list)))

(define-macro (list-clear! list)
  `(set! ,list '()))


(define (skip-lines stop)
  (let ((x (read-line-expand)))
    (cond ((eof-object? x)
	    (warn "end-of-stream while skipping input"))
	  ((not (string=? x stop))
	    (skip-lines stop)))))


;;; Assist setting of options in initialization file:

(define-macro (eval-if-mode mode . body)
  (if (and (pair? mode)
	   (= (length mode) 2)
	   (symbol? (car mode))
	   (symbol? (cadr mode)))
      (let ((tmac (car mode)) (format (cadr mode)))
        `(cond
	   ((and (or (eq? ',tmac '*)
		     (eq? ',tmac (string->symbol (substitute "m%macros%"))))
	         (or (eq? ',format '*)
		     (eq? ',format (string->symbol (substitute "%format%")))))
	     ,@body)))
      (error 'eval-if-mode "badly formed mode argument: `~a'" mode)))


;;; Macro to define a function and a predicate to manage requests that
;;; come in pairs, such as .fi/.nf.

(define-macro (define-pair func inside enter leave)
  `(begin
     (define ,inside #f)
     (define (,func on)
       (begin1
	 (if on
	     (if ,inside "" ,enter)
	     (if ,inside ,leave ""))
	 (set! ,inside on)))))


;;; Like define-pair, but for nested pairs.

(define-macro (define-nested-pair func level enter leave)
  `(begin
     (define ,level 0)
     (define (,func op)
       (case op
       (0 (begin1 (repeat-string ,level ,leave) (set! ,level 0)))
       (+ (++ ,level) ,enter)
       (- (if (zero? ,level)
	      ""
              (-- ,level) ,leave))))))



;;; --------------------------------------------------------------------------
;;; Options.

(define option-types (make-table 10))
(define option-table (make-table 100))

(define (define-option-type name check1 msg1 convert check2 msg2)
  (table-store! option-types name (list check1 msg1 convert check2 msg2)))

(define (define-option name type initial)
  (if (not (table-lookup option-types type))
      (quit "bad type `~a' for define-option" type))
  (table-store! option-table name (cons initial type)))

(define (option-setter as-event?)
  (lambda (name value)
    (let* ((opt (table-lookup option-table name))
	   (t (if opt (table-lookup option-types (cdr opt)) #f))
	   (err (lambda (msg) (quit "option `~a' requires ~a as value"
			            name msg))))
      (if opt
          (let ((val value))
	    (if as-event?
	        (begin
                  (if (not ((car t) val)) (err (cadr t)))
	          (set! val ((caddr t) (car opt) val))))
            (if (not ((cadddr t) val)) (err (car (cddddr t))))
            (set-car! opt val))
	  (quit "undefined option: `~a'" name)))))

(defevent 'option 0 (option-setter #t))
(define set-option! (option-setter #f))

(define (option name)
  (let ((opt (table-lookup option-table name)))
    (if opt (car opt) (quit "undefined option: `~a'" name))))

(define-option-type 'integer
   string? ""
   (lambda (old new) (string->number new))
   integer? "an integer")

(define-option-type 'boolean
   (lambda (x) (member x '("0" "1"))) "0 or 1"
   (lambda (old new) (string=? new "1"))
   boolean? "a boolean")

(define-option-type 'character
   (lambda (x) (= (string-length x) 1)) "a character"
   (lambda (old new) (string-ref new 0))
   char? "a character")

(define-option-type 'string
   string? ""
   (lambda (old new) new)
   string? "a string")

(define-option-type 'dynstring
  string? ""
  string-compose
  string? "a string")



;;; --------------------------------------------------------------------------
;;; Utilities for working with streams.

(define (with-i/o name proc opener setter!)
  (let* ((new (opener name)) (old (setter! new)) (result (proc)))
    (setter! old)
    (close-stream new)
    result))

(define-macro (with-output-to-stream name . body)
  `(with-i/o ,name (lambda () ,@body) open-output-stream set-output-stream!))

(define-macro (with-output-appended-to-stream name . body)
  `(with-i/o ,name (lambda () ,@body) append-output-stream set-output-stream!))

(define-macro (with-input-from-stream name . body)
  `(with-i/o ,name (lambda () ,@body) open-input-stream set-input-stream!))



;;; --------------------------------------------------------------------------
;;; Basic troff requests that are not output format specific.

(defrequest 'tm
  (lambda (tm arg)
    (display arg (error-port))
    (newline (error-port))))

(define-option 'include-files 'boolean #t)

(defrequest 'so
  (lambda (so fn)
    (cond
      ((eqv? fn "")
	(warn "missing filename for .so"))
      ((option 'include-files)
        (with-input-from-stream fn
          (copy-apply read-line-expand parse-line)))
      (else ""))))

(defrequest 'ec
  (lambda (ec c)
    (cond
      ((eqv? c "")
	(set-escape! #\\))
      ((= (string-length c) 1)
	(set-escape! (string-ref c 0)))
      (else
	(warn "non-character argument for .ec")
	(set-escape! #\\)))))

(defrequest 'rm
  (lambda (rm . names)
    (for-each
      (lambda (x)
	(defrequest x #f)
	(defstring x #f))
      names) ""))



;;; --------------------------------------------------------------------------
;;; Inline Scheme code execution; transparent output.

(define \##-env (the-environment))
(define (\##-eval expr) (eval expr \##-env))

(defrequest 'ig
  (lambda (ig delim)
    (define (copy-exec stop what)
      (let loop ((s (read-line)))
	   (cond ((eof-object? s)
		   (warn "end-of-stream during ~a" what))
		 ((not (string=? s stop))
		   (emit s)
		   (loop (read-line))))))
    (cond
      ((string=? delim "##")
	(with-output-to-stream '[##]
	  (copy-exec ".##\n" "inline Scheme execution"))
	(let ((p (open-input-string (stream->string '[##]))))
	  (copy-apply (lambda () (read p)) \##-eval)))
      ((string=? delim ">>")
	(copy-exec ".>>\n" "transparent output"))
      (else
        (skip-lines (concat #\. (if (eqv? delim "") #\. delim) #\newline))))
    ""))

(defrequest '\##
  (lambda (\## sexpr)
    (let ((p (open-input-string sexpr)))
      (copy-apply (lambda () (read p)) \##-eval))))

(defrequest '>>
  (lambda (>> code) (emit code #\newline)))



;;; --------------------------------------------------------------------------
;;; User-defined macros.

(define arg-stack '())

(defescape '$
  (lambda ($ n)
    (let ((i (string->number n)))
      (cond
	((not i)
	  (cond
	    ((string=? n "*")
	      (if (null? arg-stack) "" (apply spread (cdar arg-stack))))
	    ((string=? n "@")
	      (let loop ((a (if (null? arg-stack) '() (cdar arg-stack))))
		   (cond ((null? a)
		           "")
			 ((null? (cdr a))
			   (concat #\" (car a) #\"))
			 (else
		           (concat #\" (car a) #\" #\space (loop (cdr a)))))))
	    (else
	     (warn "invalid $ argument `~a'" n))))
	((or (null? arg-stack) (>= i (length (car arg-stack))))
	  "")
	(else (list-ref (car arg-stack) i))))))

(defnumreg '.$
  (lambda _
    (number->string (if (null? arg-stack) 0 (1- (length (car arg-stack)))))))

(define (macro-buffer-name s) (concat "[." s "]"))

(define (expand-macro . args)
  (list-push! arg-stack args)
  (with-input-from-stream (macro-buffer-name (car args))
    (copy-apply read-line-expand parse-line parse-copy-mode))
  (list-pop! arg-stack) "")

(define (copy-macro-body)
  (let* ((s (read-line-expand))
	 (t (if (eof-object? s) #f (parse-copy-mode s))))
    (cond ((not t)
	    (warn "end-of-stream during macro definition"))
	  ((not (string=? t "..\n"))
	    (emit t)
	    (copy-macro-body)))))

(defrequest 'de
  (lambda (de name)
    (cond ((eqv? name "")
	    (warn "missing name for .de"))
	  (else
            (with-output-to-stream (macro-buffer-name name)
              (copy-macro-body))
            (defmacro name expand-macro) ""))))

(defrequest 'am
  (lambda (am name)
    (cond ((eqv? name "")
	    (warn "missing name for .am"))
	  (else
            (with-output-appended-to-stream (macro-buffer-name name)
              (copy-macro-body))
            (defmacro name expand-macro) ""))))



;;; --------------------------------------------------------------------------
;;; if, if-else, else.

(defescape #\{ "")
(defescape #\} "")
(defrequest "\\}" "")    ; do not complain about .\}

(define-option 'if-true  'dynstring "to")
(define-option 'if-false 'dynstring "ne")

(define if-stack '())

(define (if-request request condition rest)
  (let* ((doit? #f)
	 (c (string-prune-left condition "!" condition))
	 (len (string-length c))
	 (neg? (not (eq? c condition))))
    (cond
      ((and (= len 1) (char-alphabetic? (string-ref c 0)))
	(cond
	  ((substring? c (option 'if-true))
	     (set! doit? #t))
	  ((substring? c (option 'if-false)))
	  (else (warn "unknown if-condition `~a'" c))))
      ((and (> len 0) (char-expression-delimiter? (string-ref c 0)))
	(let ((x (parse-expression c #f #\u)))
	  (if x (set! doit? (not (zero? x))))))
      (else
        (let ((pair (parse-pair c)))
	  (if pair
	      (set! doit? (string=? (car pair) (cdr pair)))
	      (warn "if-condition `~a' not understood" c)))))
    (cond
      ((eq? neg? doit?)
	(unread-line (concat rest #\newline))
	(skip-group))
      (else
        (unread-line (hack-if-argument rest))))
    (if (string=? request "ie")
	(list-push! if-stack (not (eq? neg? doit?))))
    ""))

;; Some people like to write .if requests such as
;;    .if t \{\
;;    .foo
;; This causes the string "\{.foo" to be passed to .if, as the first line
;; is a continuation line.  So let's strip the initial \{.  What a hack.

(define (hack-if-argument s)
  (string-prune-left s "\\{" s))

(defrequest 'if if-request)
(defrequest 'ie if-request)

(defrequest 'el
  (lambda (_ rest)
    (cond
      ((null? if-stack)
	 (warn ".el without matching .ie request"))
      ((car if-stack)
	 (unread-line (concat rest #\newline))
	 (skip-group)
	 (list-pop! if-stack))
      (else
         (unread-line (hack-if-argument rest))
	 (list-pop! if-stack)))
    ""))



;;; --------------------------------------------------------------------------
;;; Number registers.

(define numreg-table (make-table 65536))

(defrequest 'nr
  (lambda (nr name val incr)
    (cond
      ((eqv? name "")
	(warn "missing name for .nr"))
      ((eqv? val "")
	(warn "missing value for .nr"))
      (else
	(let* ((old (table-lookup numreg-table name))
	       (v (parse val))
	       (n (parse-expression v #f #\u))
	       (add? (string-prune-left v "+" #f))
	       (i (if (eqv? incr "")
		      #f
		      (parse-expression (parse incr) #f #\u))))
	  (cond
	    ((not n) "")
	    (old
	      (set-car! old (if (or add? (negative? n)) (+ (car old) n) n))
	      (if i
	          (set-cdr! old i)))
	    (else
	      (table-store! numreg-table name (cons n (if i i 0))))))))
    ""))

(defescape 'n
  (lambda (_ name . sign)
    (let ((val (table-lookup numreg-table name)))
      (cond
	(val
	  (if (not (null? sign))
	      (case (car sign)
	      (#\+  (set-car! val (+ (car val) (cdr val))))
	      (#\-  (set-car! val (- (car val) (cdr val))))))
	  (number->string (car val)))
	(else (warn "undefined number register: `~a'" name) "0")))))

(defrequest 'rr
  (lambda (rr . names)
    (for-each
      (lambda (x)
	(defnumreg x #f)
	(table-remove! numreg-table x))
      names) ""))


;;; Predefined number registers

(defnumreg 'dw
  (lambda _
    (number->string (1+ (string->number (substitute "%weekdaynum%"))))))

(defnumreg 'dy (lambda _ (substitute "%day%")))
(defnumreg 'mo (lambda _ (substitute "%month%")))
(defnumreg 'yr (lambda _ (substring (substitute "%year%") 2 4)))
(defnumreg '.C (lambda _ (if (troff-compatible?) #\1 #\0)))
(defnumreg '% #\0)
(defnumreg '.z "")
(defnumreg '.U #\1)



;;; --------------------------------------------------------------------------
;;; Strings.  Note that user-defined strings are re-scanned (strings
;;; defined via `defstring' aren't, because they may contain anything).

(defrequest 'ds
  (lambda (ds name val)
    (if (eqv? name "")
	(warn "missing name for .ds")
	(let ((v (string-prune-left val "\"" val)))
	  (defstring name (lambda _ (parse-expand v)))))
    ""))

(defrequest 'as
  (lambda (as name val)
    (if (eqv? name "")
	(warn "missing name for .as")
        (let* ((f (stringdef name))
	       (s (if f (if (string? f) f (f)) ""))
	       (new (concat s (string-prune-left val "\"" val))))
	  (defstring name (lambda _ (parse-expand new)))))
    ""))

(defescape '*
  (lambda (_ name)
    (warn "undefined string: `~a'" name)))



;;; --------------------------------------------------------------------------
;;; Now we are done with the definitions.
;;;
;;; Load the output-format-specific Scheme code and the macro-package-
;;; specific Scheme code.

(load (substitute "%directory%/scm/%format%/common.scm"))

(load (substitute "%directory%/scm/%format%/m%macros%.scm"))

(set! garbage-collect-notify? #f)

(append! load-path (list (substitute "%directory%/scm/misc")))

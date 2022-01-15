(define old-in (input-stream))

(define out (open-output-stream '/tmp/net))
(set-output-stream! out)
(emit "Hello, ")
(set-output-stream! #f)

(define out2 (open-output-stream '[out2]))
(set-output-stream! out2)
(close-stream out)

(define in (open-input-stream '/tmp/net))
(set-input-stream! in)
(emit (read-line-expand))
(emit "world!")
(set-input-stream! #f)
(close-stream in)

(set-output-stream! #f)
(close-stream out2)

(define out2 (open-input-stream '[out2]))
(defchar #\newline "")
(set-input-stream! out2)
(emit (parse (read-line-expand)))
(emit (read-line-expand))
(set-input-stream! old-in)
(close-stream out2)

(shell-command "rm /tmp/net")


(define tt (make-table 1))
(define t (make-table 100))
(table-store! tt "table" t)
(define t (table-lookup tt 'table))

(table-store! t 'bye '())
(table-remove! t 'bye)
(table-store! t 'greet "Hello,")
(table-store! t (table-lookup t 'greet) " world!\n")

(if (and (table? t) (not (table-lookup t 'bye)))
    (begin
      (display (table-lookup t 'greet))
      (display (table-lookup t (table-lookup t 'greet)))))


(define-option-type 'hello-type
  (lambda (x) (member (string-ref x 0) '(#\H #\w)))
  "oops1!!"
  (lambda (old new) (string->list new))
  (lambda (x) (member (car x) '(#\H #\w)))
  "oops2!!")

(define-option 'hello 'hello-type "oops3!!")

((eventdef 'option 0) (string-compose "helxlxo" "-x") "Hello, ")
(display (list->string (option 'hello)))
(set-option! 'hello (string->list (string-compose "wo" "+rld")))
(display (list->string (option 'hello)))
(display
  (string-prune-left (concat (repeat-string 2 "!") #\newline) "!" "oops"))

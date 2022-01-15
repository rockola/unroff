;;;; -*-Scheme-*-
;;;;
;;;; $Revision: 1.4 $
;;;;
;;;; Definitions for HTML output format to be loaded when no -mfoo
;;;; option has been given.


;;; --------------------------------------------------------------------------
;;; Simple default start and exit handler.

(defevent 'start 10
  (lambda _
    (let* ((docname (option 'document))
	   (title   (option 'title))
	   (t       (if title title (if docname docname "(untitled)"))))
      (if docname
	  (set-output-stream! (open-output-stream (concat docname ".html"))))
      (emit-HTML-prolog)
      (emit "<title>" (translate t) "</title>\n</head>\n<body>\n"))))

(defevent 'exit 10
  (lambda _
    (emit (change-font "R") (preform #f))
    (emit "</body>\n</html>\n")
    (close-stream (set-output-stream! #f))))

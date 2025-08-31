(defpackage :son
  (:use :cl :alexa :cl-ppcre)
  (:export #:read-all-lines #:read-file-as-string #:lex
           #:son-object))

(in-package :son)


(defun read-file-as-string (file)
  (with-open-file (stream file :direction :input :element-type 'character)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))


(defun read-all-lines (file)
  (with-open-file (stream file :direction :input)
    (loop for ln = (read-line stream nil)
          collect ln)))

(deftype token ()
  `(cons keyword t))

(defun tok (type &optional val)
  (cons type val))

(define-string-lexer son-lexer
  ((:num "\\d+")
   (:ident "[A-Za-z][A-Za-z0-9_-]*"))
  ("\\(" (return (tok :obj-start)))
  ("\\)" (return (tok :obj-end)))
  ("\\[" (return (tok :list-start)))
  ("\\]" (return (tok :list-end)))
  ("\\:" (return (tok :colon)))
  ("\\;" (return (tok :semicol)))
  ("{{IDENT}}" (return (tok :ident (intern $@))))
  ("{{NUM}}" (return (tok :number (parse-integer $@))))
  ("\\s+" nil)) 

(defun lex (string)
  (loop with lexer := (son-lexer string)
        for tok := (funcall lexer)
        while tok
        collect tok))


(defclass son-object ()
  ((fields
    :initarg :fields
    :accessor fields)))

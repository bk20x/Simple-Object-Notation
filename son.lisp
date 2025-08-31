(defpackage :son
  (:use :cl :alexa :cl-ppcre :parse-float)
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
  ((:num "\\b(?:\\d+\\.\\d+|\\d+|\\.\\d+)\\b")
   (:name "[A-Za-z][A-Za-z0-9_-]*"))
  ("\\(" (return (tok :obj-start)))
  ("\\)" (return (tok :obj-end)))
  ("\\[" (return (tok :list-start)))
  ("\\]" (return (tok :list-end)))
  ("\\:" (return (tok :colon)))
  ("\\;" (return (tok :semicol)))
  ("{{NAME}}" (return (tok :ident (intern $@))))
  ("{{NUM}}" (return (tok :number (parse-float $@))))
  ("\\s+" nil)) 

(defun lex (string)
  (loop with lexer := (son-lexer string)
        for tok := (funcall lexer)
        while tok
        collect tok))


(defclass son-object ()
  ((fields            ;; `fields` is son-object-map, k, v
    :initarg :fields
    :accessor fields)))


(defclass son-list ()
  ((elems
    :initarg :elems
    :accessor elems)))


(deftype son-object-map ()
  '(and hash-table
    (satisfies son-object-map-p)))

(defun son-object-map-p (table)
  (and (hash-table-p table)
       (loop for k being the hash-keys of table
             always (typep k 'string))
       (loop for v being the hash-values of table
             always (typep v 'son-object))))


;(defun parse-son-object ()
;())

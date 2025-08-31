(defpackage :son
  (:use :cl :alexa :cl-ppcre :parse-float)
  (:export #:read-all-lines #:read-file-as-string #:lex #:get-symbol #:filter
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


(defun filter (pred xs)
  (let ((result nil))
    (loop for x in xs
          when (funcall pred x)
          do (push x result))
    (if (= 1 (length result))
        (car result)
        (nreverse result))))


(deftype token ()
  `(cons keyword t))

(defun tok (type &optional val)
  (cons type val))

(defun get-symbol (token)
  (declare (type token token))
   (car token))

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


(defclass son-object () ;;impl for son-object as `(foo: bar; baz: 69)`
  ((fields              ;;`fields` is Hash-Table[string :: T]
    :initarg :fields
    :accessor fields)))



(defclass son-list (son-object)
  ((elems
    :initarg :elems
    :accessor elems)))






;(defun parse-son-object ()
;())

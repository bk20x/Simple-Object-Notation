(defpackage :son
  (:use :cl :alexa  :parse-float)
  (:export #:read-all-lines #:read-file-as-string #:lex #:get-symbol #:filter
           #:parse-toks
           #:son-object #:son-list #:make-son-object #:fields #:elems))

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

(defun get-val (token)
  (declare (type token token))
  (cdr token))

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

(defun make-son-object ()
  (make-instance 'son-object))

(defclass son-list ()
  ((elems             ;;`elems` is List[son-object]
    :initarg :elems
    :accessor elems)))



(defun parse-toks (tokens)
  "Parses a list of tokens into a son-object or son-list. ^-^"
  (let ((current-toks tokens))
    (labels ((next-token ()
               "Gets next token and advances position. :3"
               (if current-toks
                   (prog1 (car current-toks)
                     (setf current-toks (cdr current-toks)))
                   nil))
             (peek-token ()
               "Checks next token without consuming. ^_^"
               (car current-toks))
             (match-token (expected)
               "Consumes next token and errors if its type doesn't match. :P"
               (let ((token (next-token)))
                 (unless (and token (eql (get-symbol token) expected))
                   (error "Expected token of type ~a but got ~a" expected token))
                 token))
             (parse-object ()
               "Returns a son-object instance. (ᴗ˳ᴗ)ᶻ𝗓"
               (match-token :obj-start)
               (let ((ht (make-hash-table :test 'equal)))
                 (loop while (and (peek-token) (not (eql (get-symbol (peek-token)) :obj-end)))
                       do
                       (let* ((key-token (match-token :ident))
                              (key (get-val key-token)))
                         (match-token :colon)
                         (let ((val (parse-value)))
                           (setf (gethash key ht) val)))
                       (when (and (peek-token) (eql (get-symbol (peek-token)) :semicol))
                         (next-token)))
                 (match-token :obj-end)
                 (make-instance 'son-object :fields ht)))
             (parse-list ()
               "Parses a list of objects and returns a son-list instance. :D"
               (match-token :list-start)
               (let ((items nil))
                 (loop while (and (peek-token) (not (eql (get-symbol (peek-token)) :list-end)))
                       do (setf items (nconc items (list (parse-value)))))
                 (match-token :list-end) 
                 (let ((items-list
                         (remove-if #'null items)))
                   (make-instance 'son-list :elems items-list))))
             (parse-value ()
               "Parses the next token whether it's a list, object, or simple value. >_<"
               (let ((token (peek-token)))
                 (cond
                   ((eql (get-symbol token) :obj-start) (parse-object))
                   ((eql (get-symbol token) :list-start) (parse-list))
                   (t (let ((simple-value-token (next-token)))
                        (if simple-value-token
                            (get-val simple-value-token)
                            (error "Unexpected end of tokens while parsing simple value. 0_o"))))))))
      (if (peek-token)
          (let ((result (case (get-symbol (peek-token))
                          (:obj-start (parse-object))
                          (:list-start (parse-list))
                          (t (error "Expected object or list at top level <_<."))))
                (remaining-toks current-toks))
            (if remaining-toks
                (error "Unexpected tokens at end of stream: ~a *_*" remaining-toks))
            result)
          (error "Empty token stream. 7_7")))))

(defpackage :son
  (:use :cl :alexa :parse-float)
  (:export #:read-all-lines #:read-file-as-string #:lex #:get-symbol #:filter #:parse-toks
           #:son-object #:son-list  #:fields #:field #:elems #:elem #:to-class #:keys))

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


(defun parse-bool (str)
  (declare (type simple-string str)
           (optimize (speed 3) (safety 0)))
  (if (string= str "true")
      t))


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


(define-string-lexer *son-lexer*
  ((:sint "^-?\\d+")
  (:sfloat"[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?")  ;; got me feeling like larry wall
  (:name "[A-Za-z][A-Za-z0-9_-]*")
  (:bool "true|false"))
  ("\\(" (return (tok :obj-start)))
  ("\\)" (return (tok :obj-end)))
  ("\\[" (return (tok :list-start)))
  ("\\]" (return (tok :list-end)))
  ("\\:" (return (tok :colon)))
  ("\\;" (return (tok :semicol)))
  ("{{BOOL}}"   (return (tok :bool (parse-bool $@))))
  ("{{SINT}}"   (return (tok :int (parse-integer $@))))
  ("{{SFLOAT}}" (return (tok :float (parse-float $@))))
  ("{{NAME}}"   (return (tok :ident $@)))
  ("\\s+" nil)) 

(defun lex (str)
  (declare (type string str)
    (optimize (speed 3)))
  (loop with lexer := (*son-lexer* str)
        for tok := (funcall lexer)
        while tok
        collect tok))


(defclass son-object ()
  ((fields
    :initarg :fields
    :accessor fields
    :type hash-table)))


(defclass son-list ()
  ((elems
    :initarg :elems
    :accessor elems
    :type list)))

(defun parse-toks (tokens)
  "Parses a list of tokens into a son-object or son-list. ^-^"
    (declare (type list tokens)
      (optimize (speed 3)))
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
                 (unless (and token
                   (eql (get-symbol token) expected))
                    (error "Expected token of type ~a but got ~a" expected token))
                     token))
             (parse-object ()
               "Returns a son-object instance. (ᴗ˳ᴗ)ᶻ𝗓"
              (match-token :obj-start)
               (let ((ht (make-hash-table :test 'equal))) 
                 (loop while (and (peek-token)
                   (not (eql (get-symbol (peek-token)) :obj-end)))
                       do
                       (let* ((key-token (match-token :ident))
                              (key (get-val key-token)))
                         (match-token :colon)
                          (let ((val (parse-token)))
                           (setf (gethash key ht) val)))
                          (when
                            (and
                             (peek-token)
                              (eql (get-symbol (peek-token)) :semicol))
                               (next-token)))
                   (match-token :obj-end)
                    (make-instance 'son-object :fields ht)))
             (parse-list ()
               "Parses a list of objects and returns a son-list. :D"
              (match-token :list-start)
               (let ((items nil))
                 (loop while (and(peek-token)
                   (not (eql (get-symbol (peek-token)) :list-end)))
                     do
                      (setf items
                       (nconc items (list (parse-token)))))
                 (match-token :list-end) 
                 (let ((items-list
                         (remove-if #'null items)))
                           (make-instance 'son-list :elems items-list))))
             (parse-token ()
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
            (let ((result
                     (case (get-symbol (peek-token))
                          (:obj-start (parse-object))
                          (:list-start (parse-list))
                          (t (error "Expected object or list at top level <_<."))))
           (remaining-toks current-toks))
            (if remaining-toks
                (error "Unexpected tokens at end of stream: ~a *_*" remaining-toks))
                  result)
             
          (error "Empty token stream. 7_7")))))





(defun field (field obj)
  (gethash field
      (fields obj)))

(defun elem (idx slist)
  (elt (elems slist) idx))

(defun keys (obj)
  (loop for k being the hash-keys of (fields obj)
        collect k))




(defun class-slot-names (class)
 (mapcar #'closer-mop:slot-definition-name
  (closer-mop:class-slots (find-class class))))


(defun to-class (obj class)
 (let ((result (make-instance class)))
  (loop for k in (keys obj) do
   (let ((sym (read-from-string k))) 
    (if (slot-exists-p result sym)
        (setf (slot-value result sym) (field k obj))
 (error "Unknown field ~a for class ~a" sym class))))
         result))

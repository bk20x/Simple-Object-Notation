(defpackage :son
  (:use :cl :alexa :parse-float)
  (:export #:read-all-lines #:read-file-as-string #:lex #:get-symbol #:filter #:parse-token-list #:to-son #:to-class
           #:son-object #:son-list #:fields #:field #:elems #:elem #:keys))

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
  ((:int "^-?\\d+")
  (:float"[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?")  ;; got me feeling like larry wall
  (:name "[A-Za-z][A-Za-z0-9_-]*")
  (:bool "true|false"))
  ("\\(" (return (tok :obj-start)))
  ("\\)" (return (tok :obj-end)))
  ("\\[" (return (tok :list-start)))
  ("\\]" (return (tok :list-end)))
  ("\\:" (return (tok :colon)))
  ("\\;" (return (tok :semicol)))
  ("{{BOOL}}"   (return (tok :bool (parse-bool $@))))
  ("{{INT}}"   (return (tok :int (parse-integer $@))))
  ("{{FLOAT}}" (return (tok :float (parse-float $@))))
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

(defun parse-token-list (tokens)
    (declare (type list tokens)
      (optimize (speed 3)))
  (let ((current-toks tokens))
    (labels ((next-token ()
               (if current-toks
                   (prog1 (car current-toks) 
                     (setf current-toks (cdr current-toks)))
                       nil))
             (peek-token ()
               (car current-toks))
             (match-token (expected)
               (let ((token (next-token)))
                 (unless (and token
                   (eql (get-symbol token) expected))
                    (error "Expected token of type ~a but got ~a" expected token))
                     token))
             (parse-object ()
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


(defun to-class (class obj)
  (let ((result (make-instance class))
        (pkg (package-name (symbol-package class))))
  (loop for slot in (keys obj) do
   (let ((sym (read-from-string (concatenate 'string pkg "::"  slot))))
    (if (slot-exists-p result sym)
        (setf (slot-value result sym) (field slot obj))
 (error "Unknown field ~a for class ~a" sym class))))
         result))





(defun slot-names (class)
  (mapcar
   #'closer-mop:slot-definition-name
   (closer-mop:class-slots (find-class class))))


(defun to-table (obj)
  (declare (optimize (speed 3) (safety 0)))
  (let ((result (make-hash-table :test 'equal)))
    (mapcar
     (lambda (s)
       (setf (gethash s result) (slot-value obj s)))
     (slot-names (class-name (class-of obj))))
    result))


(defmethod to-son ((obj standard-object))
  (declare (optimize (speed 3) (safety 0))
          (type standard-object obj))
  (let* ((table (to-table obj))
         (output
           (with-output-to-string (stream)
             (loop for k being the hash-keys of table
                   do (format stream "~a: ~a; " k (let ((val (gethash k table)))
                                                    (to-son val)))))))
    (format nil "(~a)" (string-trim " " output))))


(defmethod to-son ((obj list))
  (format nil "[~{~a;~^ ~}]" obj))


(defmethod to-son ((obj integer))
  obj)

(defmethod to-son ((obj float))
  obj)

(defmethod to-son ((obj string))
  obj)



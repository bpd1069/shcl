(in-package :shcl.lexer)

(optimization-settings)

(defmacro define-make-load-form-for-class (class-name)
  (let ((object (gensym "OBJECT"))
        (environment (gensym "ENVIRONMENT")))
    `(defmethod make-load-form ((,object ,class-name) &optional ,environment)
       (declare (ignore ,object ,environment))
       `(error "make-load-form called"))))

(defclass token ()
  ((value :type (or null string)
          :initform nil
          :accessor token-value
          :initarg :value)))
(defmethod print-object ((token token) stream)
  (format stream "#<~A ~W>" (class-name (class-of token)) (token-value token)))
(define-make-load-form-for-class token)

(defclass a-word (token)
  ())
(define-make-load-form-for-class a-word)

(defclass simple-word (a-word)
  ((text
    :initarg :text
    :accessor simple-word-text
    :initform (required)
    :type string)))
(defmethod print-object ((simple-word simple-word) stream)
  (format stream "#<~A ~S>" (class-name (class-of simple-word)) (simple-word-text simple-word)))
(define-make-load-form-for-class simple-word)

(defclass assignment-word (a-word)
  ((name
    :type name
    :initform (required)
    :accessor assignment-word-name
    :initarg :name)
   (value-word
    :type a-word
    :initarg :value-word
    :initform (required)
    :accessor assignment-word-value-word)))
(defmethod print-object ((word assignment-word) stream)
  (format stream "#<~A ~S = ~S>" (class-name (class-of word)) (assignment-word-name word) (assignment-word-value-word word)))
(define-make-load-form-for-class assignment-word)

(defclass name (simple-word)
  ())
(define-make-load-form-for-class name)

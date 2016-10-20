(in-package :shcl.lexer)

(shcl.utility::optimization-settings)

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
(define-make-load-form-for-class token)

(defclass a-word (token)
  ())
(define-make-load-form-for-class a-word)

(defclass simple-word (a-word)
  ((text
    :initarg :text
    :accessor simple-word-text
    :initform (error "required")
    :type string)))
(define-make-load-form-for-class simple-word)

(defclass assignment-word (a-word)
  ((name
    :type name
    :initform (error "required")
    :accessor assignment-word-name
    :initarg :name)
   (value-word
    :type a-word
    :initarg :value-word
    :initform (error "required")
    :accessor assignment-word-value-word)))
(define-make-load-form-for-class assignment-word)

(defclass name (simple-word)
  ())
(define-make-load-form-for-class name)

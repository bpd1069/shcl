(in-package :shcl.lexer)

(shcl.utility::optimization-settings)

(defmacro define-make-load-form-for-class (class-name)
  (let ((object (gensym "OBJECT"))
        (environment (gensym "ENVIRONMENT")))
    `(defmethod make-load-form ((,object ,class-name) &optional ,environment)
       (declare (ignore ,object ,environment))
       `(error "make-load-form called"))))

(defclass token ()
  ((value :initarg :value)))

(defclass a-word (token)
  ())

(defclass simple-word (a-word)
  ((text
    :initarg :text
    :accessor simple-word-text)))

(defclass assignment-word (a-word)
  ((name
    :initarg :name)
   (value-word
    :type a-word ;; Can't remove this
    :initarg :value-word
    )))
(define-make-load-form-for-class assignment-word) ;; Can't remove this

(defclass name (simple-word)
  ())

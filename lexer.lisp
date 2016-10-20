(in-package :shcl.lexer)

(shcl.utility::optimization-settings)

(defmacro define-make-load-form-for-class (class-name)
  (let ((object (gensym "OBJECT"))
        (environment (gensym "ENVIRONMENT")))
    `(defmethod make-load-form ((,object ,class-name) &optional ,environment)
       (declare (ignore ,object ,environment))
       `(error "make-load-form called"))))

(defclass token ()
  ((value))) ;; Can't remove this slot

(defclass a-word (token)
  ())

(defclass simple-word (a-word)
  ())

(defclass assignment-word (a-word)
  ((value-word
    :type a-word ;; Can't remove this
    :initarg :value-word
    )))
(define-make-load-form-for-class assignment-word) ;; Can't remove this

(defclass name (simple-word)
  ())

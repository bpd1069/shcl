(in-package :shcl.lexer)

(shcl.utility::optimization-settings)

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

(defmethod make-load-form ((object assignment-word) &optional environment)
  (declare (ignore environment))
  (error "make-load-form called"))

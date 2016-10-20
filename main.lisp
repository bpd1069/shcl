(in-package :shcl)

(shcl.utility::optimization-settings)

(defgeneric to-string (thing))
(defmethod to-string ((thing string))
  thing)
(defmethod to-string ((thing shcl.lexer::simple-word))
  nil)
(defmethod to-string ((thing shcl.lexer::token))
  nil)

(defun main ()
  (to-string "echo")
  (make-instance 'shcl.lexer::assignment-word
                 :value-word (make-instance 'shcl.lexer::simple-word)
                 ))

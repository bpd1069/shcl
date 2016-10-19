(in-package :shcl)

(optimization-settings)

(defgeneric to-string (thing))
(defmethod to-string ((thing string))
  thing)
(defmethod to-string ((thing shcl.lexer::simple-word))
  (shcl.lexer::simple-word-text thing))
(defmethod to-string ((thing shcl.lexer::token))
  nil)

(defun main ()
  (to-string "echo")
  (shcl.lexer::tokenize "FOO=123"))

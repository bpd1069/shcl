(in-package :shcl)

(optimization-settings)

(defun main ()
  (shcl.expand::to-string "echo")
  (shcl.lexer::tokenize "FOO=123"))

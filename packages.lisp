(defpackage :shcl.utility
  (:use :common-lisp))

(defpackage :shcl.lexer
  (:use :common-lisp :shcl.utility))

(defpackage :shcl
  (:use :common-lisp :shcl.lexer :shcl.utility))

(defsystem "shcl"
  :description "Shcl, a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "All rights reserved."
  :depends-on ("alexandria" "trivial-garbage" "cl-fad" "bordeaux-threads" "fset" "cl-unicode" "closer-mop" "cl-cli" "trivial-gray-streams")
  :components ((:file "packages")
               (:file "utility" :depends-on ("packages"))
               (:file "thread" :depends-on ("packages" "utility"))
               (:file "lexer" :depends-on ("packages" "utility"))
               (:file "parser" :depends-on ("packages" "utility" "lexer"))
               (:file "shell-grammar" :depends-on ("packages" "utility" "parser" "lexer"))
               (:file "posix" :depends-on ("packages" "utility"))
               (:file "fork-exec" :depends-on ("packages" "utility" "shell-grammar" "posix"))
               (:file "environment" :depends-on ("packages" "utility" "posix"))
               (:file "baking" :depends-on ("packages" "utility" "thread"))
               (:file "builtin" :depends-on ("packages" "utility"))
               (:file "main" :depends-on ("packages" "shell-grammar" "lexer" "baking" "utility"))))

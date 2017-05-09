(defsystem "shcl-test"
  :description "Shcl tests, tests for a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "Modified BSD License"
  :depends-on ("prove" "shcl")
  :components
  ((:file "test/lexer")
   (:file "test/utility")
   (:file "test/environment")
   (:file "test/posix")
   (:file "test/lisp-interpolation")
   (:file "test/data")
   (:file "test/report"))
  :perform (test-op :after (op c)
                    (let (failed)
                      (dolist (p (list-all-packages))
                        (when (and
                               (string-equal (package-name p) "SHCL-TEST/"
                                             :end1 (min (length (package-name p))
                                                        #.(length "SHCL-TEST/")))
                               (not (equal (package-name p)
                                           "SHCL-TEST/REPORT")))
                          (unless (funcall (intern "RUN-TEST-PACKAGE" :prove) p)
                            (setf failed t))))
                      (setf (symbol-value (intern "*LAST-TEST-PASSED*" :shcl-test/report)) (not failed)))))

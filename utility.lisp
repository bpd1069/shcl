(in-package :shcl.utility)

(defmacro optimization-settings ()
  "Declaims standard optimization settings.

Put this at the top of every file!"
  `(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0))))

(optimization-settings)

(defun observe-dump ()
  ;; For some reason, calling this function before dumping is
  ;; important.
  )

(define-condition required-argument-missing (error)
  ()
  (:documentation
   "A condition for indicating that a required argument was not provided."))

(defmacro required ()
  "This form always signals an error of type `required-argument-missing'."
  `(error 'required-argument-missing))

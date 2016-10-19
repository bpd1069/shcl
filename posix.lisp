(in-package :shcl.posix)

(optimization-settings)

(defvar errno 0)

(define-condition syscall-error (error)
  ((errno
    :initform errno
    :accessor syscall-error-errno
    :type integer)
   (function
    :initform nil
    :initarg :function
    :accessor syscall-error-function))
  (:report (lambda (c s)
             (format s "Encountered an error (~A) in ~A.  ~A"
                     (syscall-error-errno c)
                     (syscall-error-function c)
                     "Unknown"))))

(defun pass (value)
  (declare (ignore value))
  t)

(defun not-negative-p (number)
  (not (minusp number)))

(defun not-negative-1-p (number)
  (not (equal -1 number)))

(defmacro define-c-wrapper ((lisp-name c-name) &rest r)
  (declare (ignore c-name r))
  `(defun ,lisp-name (&rest args)
     (declare (ignore args))
     (error "Attempted to use CFFI")))

(define-c-wrapper (posix-spawnp "posix_spawnp") (:int #'zerop)
  (pid (:pointer pid-t))
  (file :string)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t)))
  (attrp (:pointer (:struct posix-spawnattr-t)))
  (argv string-table)
  (envp string-table))

(define-c-wrapper (posix-spawn-file-actions-init "posix_spawn_file_actions_init") (:int #'zerop)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t))))

(define-c-wrapper (posix-spawn-file-actions-destroy "posix_spawn_file_actions_destroy") (:int #'zerop)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t))))

(defmacro with-posix-spawn-file-actions ((symbol) &body body)
  (declare (ignore symbol body))
  `(error "Attempted to use CFFI"))

(define-c-wrapper (posix-spawn-file-actions-addclose "posix_spawn_file_actions_addclose") (:int #'zerop)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t)))
  (fildes :int))

(define-c-wrapper (posix-spawn-file-actions-addopen "posix_spawn_file_actions_addopen") (:int #'zerop)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t)))
  (fildes :int)
  (path :string)
  (oflag :int)
  (mode mode-t))

(define-c-wrapper (posix-spawn-file-actions-adddup2 "posix_spawn_file_actions_adddup2") (:int #'zerop)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t)))
  (fildes :int)
  (newfildes :int))


(define-c-wrapper (posix-spawnattr-init "posix_spawnattr_init") (:int #'zerop)
  (attr (:pointer (:struct posix-spawnattr-t))))

(define-c-wrapper (posix-spawnattr-destroy "posix_spawnattr_destroy") (:int #'zerop)
  (attr (:pointer (:struct posix-spawnattr-t))))

(defmacro with-posix-spawnattr ((symbol) &body body)
  (declare (ignore symbol body))
  `(error "Attempted to use CFFI"))

(defun environment-iterator ()
  (make-iterator ()
    (stop)))

(define-c-wrapper (opendir "opendir") (:pointer (lambda (x) (not (null-pointer-p x))))
  (name :string))

(define-c-wrapper (closedir "closedir") (:int #'zerop)
  (dirp :pointer))

(define-c-wrapper (dirfd "dirfd") (:int #'not-negative-p)
  (dirp :pointer))

(define-c-wrapper (%readdir "readdir") ((:pointer (:struct dirent))
                                        (lambda (x)
                                          (or (not (null-pointer-p x))
                                              (zerop errno))))
  (dirp :pointer))

(defun readdir (dirp)
  (%readdir dirp))

(defun open-fds ()
  #())

(defun compiler-owned-fds ()
  #+sbcl
  (vector (sb-sys:fd-stream-fd sb-sys:*tty*))
  #-sbcl
  (progn
    (warn "Unsupported compiler.  Can't determine which fds the compiler owns.")
    #()))

(defun fork ()
  #+sbcl (sb-posix:fork)
  #-sbcl (error "Cannot fork on this compiler"))

(define-c-wrapper (_exit "_exit") (:void)
  (status :int))

(define-c-wrapper (exit "exit") (:void)
  (status :int))

(define-c-wrapper (%waitpid "waitpid") (pid-t #'not-negative-1-p)
  (pid pid-t)
  (wstatus (:pointer :int))
  (options :int))

(defun waitpid (&rest r)
  (declare (ignore r))
  0)

(defmacro forked (&body body)
  (let ((pid (gensym "PID"))
        (e (gensym "E")))
    `(let ((,pid (fork)))
       (cond
         ((plusp ,pid)
          ,pid)
         ((zerop ,pid)
          (unwind-protect
               (handler-case (progn ,@body)
                 (error (,e)
                   (format *error-output* "ERROR: ~A~%" ,e)
                   (finish-output *error-output*)
                   (_exit 1)))
            (_exit 0)))
         ((minusp ,pid)
          ;; The wrapper around posix fork should have taken care of this
          ;; for us
          (assert nil nil "This is impossible"))))))

(define-c-wrapper (dup "dup") (:int #'not-negative-1-p)
  (fd :int))

(define-c-wrapper (getpid "getpid") (pid-t))

(define-c-wrapper (%open "open") (:int #'not-negative-1-p)
  (pathname :string)
  (flags :int)
  &rest)

(defun posix-open (pathname flags &optional mode)
  (if mode
      (%open pathname flags mode)
      (%open pathname flags)))

(define-c-wrapper (%openat "openat") (:int #'not-negative-1-p)
  (dirfd :int)
  (pathname :string)
  (flags :int)
  &rest)

(defun openat (dirfd pathname flags &optional mode)
  (if mode
      (%openat dirfd pathname flags mode)
      (%openat dirfd pathname flags)))

(define-c-wrapper (fcntl "fcntl") (:int #'not-negative-1-p)
  (fildes :int)
  (cmd :int)
  &rest)

(define-c-wrapper (%close "close") (:int #'not-negative-1-p)
  (fd :int))

(defun posix-close (fd)
  (%close fd))

(define-c-wrapper (%pipe "pipe") (:int #'not-negative-1-p)
  (fildes (:pointer :int)))

(defun pipe ()
  (%pipe))

#-sbcl
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library shcl-support
        (:linux (:default "shcl-support"))))

  (defcfun (%wifexited "wifexited" :library shcl-support) :int
    (status :int))
  (defun wifexited (status)
    (not (zerop (%wifexited status))))

  (defcfun (%wifstopped "wifstopped" :library shcl-support) :int
    (status :int))
  (defun wifstopped (status)
    (not (zerop (%wifstopped status))))

  (defcfun (%wifsignaled "wifsignaled" :library shcl-support) :int
    (status :int))
  (defun wifsignaled (status)
    (not (zerop (%wifsignaled status))))

  (defcfun (wexitstatus "wexitstatus" :library shcl-support) :int
    (status :int))

  (defcfun (wtermsig "wtermsig" :library shcl-support) :int
    (status :int))

  (defcfun (wstopsig "wstopsig" :library shcl-support) :int
    (status :int)))

#+sbcl
(progn
  (defmacro define-wrapper (symbol base)
    (declare (ignore base))
    `(defun ,symbol (&rest r)
       (declare (ignore r))))

  (define-wrapper wifexited sb-posix:wifexited)
  (define-wrapper wifstopped sb-posix:wifstopped)
  (define-wrapper wifsignaled sb-posix:wifsignaled)
  (define-wrapper wexitstatus sb-posix:wexitstatus)
  (define-wrapper wtermsig sb-posix:wtermsig)
  (define-wrapper wstopsig sb-posix:wstopsig))

(define-c-wrapper (%strerror "strerror") (:string)
  (err :int))

(defvar *errno-lock* (make-lock))

(defun strerror (err)
  (with-lock-held (*errno-lock*)
    (%strerror err)))

;; Copyright 2017 Bradley Jensen
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(defpackage :shcl/core/support
  (:use :common-lisp :cffi :shcl/core/utility :shcl/core/posix-types)
  (:export
   #:wifexited #:wifstopped #:wifsignaled #:wexitstatus #:wtermsig #:wstopsig
   #:string-table #:fd-actions #:make-fd-actions #:fd-actions-add-close
   #:fd-actions-add-dup2 #:shcl-spawn #:s-isreg #:s-isdir #:s-ischr #:s-isblk
   #:s-isfifo #:s-islnk #:s-issock))
(in-package :shcl/core/support)

(optimization-settings)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library shcl-support
    (:linux (:default "libshcl-support") :search-path ".")))

(use-foreign-library shcl-support)

(defcfun (wifexited "wifexited" :library shcl-support) (:boolean :int)
  (status :int))

(defcfun (wifstopped "wifstopped" :library shcl-support) (:boolean :int)
  (status :int))

(defcfun (wifsignaled "wifsignaled" :library shcl-support) (:boolean :int)
  (status :int))

(defcfun (wexitstatus "wexitstatus" :library shcl-support) :int
  (status :int))

(defcfun (wtermsig "wtermsig" :library shcl-support) :int
  (status :int))

(defcfun (wstopsig "wstopsig" :library shcl-support) :int
  (status :int))

(defcfun (s-isreg "s_isreg") (:boolean :int)
  (mode mode-t))

(defcfun (s-isdir "s_isdir") (:boolean :int)
  (mode mode-t))

(defcfun (s-ischr "s_ischr") (:boolean :int)
  (mode mode-t))

(defcfun (s-isblk "s_isblk") (:boolean :int)
  (mode mode-t))

(defcfun (s-isfifo "s_isfifo") (:boolean :int)
  (mode mode-t))

(defcfun (s-islnk "s_islnk") (:boolean :int)
  (mode mode-t))

(defcfun (s-issock "s_issock") (:boolean :int)
  (mode mode-t))

(define-foreign-type string-table-type ()
  ((size
    :initarg :size
    :initform nil))
  (:actual-type :pointer)
  (:simple-parser string-table))

(defmethod translate-to-foreign ((sequence fset:seq) (type string-table-type))
  (with-slots (size) type
    (let ((seq-size (fset:size sequence))
          table
          side-table)
      (when size
        (assert (equal seq-size size)))
      (let (success)
        (unwind-protect
             (let ((index 0))
               (setf table (foreign-alloc '(:pointer :char) :initial-element (null-pointer) :count (1+ seq-size)))
               (setf side-table (make-array seq-size :initial-element nil))
               (fset:do-seq (thing sequence)
                 (multiple-value-bind (converted-value param) (convert-to-foreign thing :string)
                   (setf (mem-aref table '(:pointer :char) index) converted-value)
                   (setf (aref side-table index) param)
                   (incf index)))
               (setf success t)
               (values table side-table))
          (unless success
            (if side-table
                (free-translated-object table type side-table)
                (foreign-free table))))))))

(defmethod translate-to-foreign ((sequence list) (type string-table-type))
  (translate-to-foreign (fset:convert 'fset:seq sequence) type))

(defmethod translate-to-foreign ((sequence vector) (type string-table-type))
  (translate-to-foreign (fset:convert 'fset:seq sequence) type))

(defmethod free-translated-object (translated (type string-table-type) param)
  (loop :for index :below (length param) :do
     (unless (null-pointer-p (mem-aref translated '(:pointer :char) index))
       (free-converted-object (mem-aref translated '(:pointer :char) index) :string (aref param index))))
  (foreign-free translated))

(defcfun (%%make-fd-actions "make_shcl_fd_actions") :pointer)
(defcfun (%%destroy-fd-actions "destroy_shcl_fd_actions") :void
  (actions :pointer))
(defcfun (%%fd-actions-add-close "shcl_fd_actions_add_close") :void
  (actions :pointer)
  (fd :int))
(defcfun (%%fd-actions-add-dup2 "shcl_fd_actions_add_dup2") :void
  (actions :pointer)
  (fd1 :int)
  (fd2 :int))

(defstruct (fd-actions
             (:constructor %make-fd-actions))
  (actions (make-extensible-vector)))

(defun make-fd-actions ()
  (%make-fd-actions))

(defstruct fd-action-close
  fd)

(defun fd-actions-add-close (actions fd)
  (vector-push-extend (make-fd-action-close :fd fd) (fd-actions-actions actions)))

(defstruct fd-action-dup2
  fd1
  fd2)

(defun fd-actions-add-dup2 (actions fd1 fd2)
  (vector-push-extend (make-fd-action-dup2 :fd1 fd1 :fd2 fd2) (fd-actions-actions actions)))

(define-foreign-type fd-actions-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser fd-actions))

(defmethod translate-to-foreign ((lisp-actions fd-actions) (type fd-actions-type))
  (let ((actions (%%make-fd-actions)))
    (loop :for action :across (fd-actions-actions lisp-actions) :do
       (etypecase action
         (fd-action-close
          (%%fd-actions-add-close actions (fd-action-close-fd action)))
         (fd-action-dup2
          (%%fd-actions-add-dup2 actions (fd-action-dup2-fd1 action) (fd-action-dup2-fd2 action)))))
    actions))

(defmethod free-translated-object (translated (type fd-actions-type) param)
  (declare (ignore param type))
  (%%destroy-fd-actions translated))

(defcfun (%shcl-spawn "shcl_spawn" :library shcl-support) :int
  (pid (:pointer pid-t))
  (path :string)
  (search :int)
  (working-directory-fd :int)
  (fd-actions fd-actions)
  (argv string-table)
  (envp string-table))

(defun shcl-spawn (path search-p working-directory-fd fd-actions argv envp)
  (with-foreign-object (pid 'pid-t)
    (let ((result (%shcl-spawn pid path (if search-p 1 0) working-directory-fd fd-actions argv envp)))
      (when (zerop result)
        (error "spawn failed"))
      (mem-ref pid 'pid-t))))

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

(defpackage :shcl/core/shell-grammar
  (:use
   :common-lisp :shcl/core/parser :shcl/core/lexer :shcl/core/utility
   :shcl/core/iterator)
  (:import-from :shcl/core/advice #:define-advice)
  (:export
   #:command-iterator
   ;; nonterminals
   #:complete-command #:command-list #:command-list-tail #:and-or #:and-or-tail
   #:pipeline #:pipe-sequence #:pipe-sequence-tail #:command #:compound-command
   #:subshell #:compound-list #:term #:term-tail #:for-clause #:name-nt #:in-nt
   #:wordlist #:wordlist-tail #:case-clause #:case-list-ns #:case-list
   #:case-list-tail #:case-item-ns #:case-item #:pattern #:pattern-tail
   #:if-clause #:else-part #:while-clause #:until-clause #:function-definition
   #:function-body #:fname #:brace-group #:do-group #:simple-command #:cmd-name
   #:cmd-word #:cmd-prefix #:cmd-prefix-tail #:cmd-suffix #:cmd-suffix-tail
   #:redirect-list #:redirect-list-tail #:io-redirect #:io-file #:filename
   #:io-source #:io-here #:here-end #:newline-list #:newline-list-tail
   #:linebreak #:separator-op #:separator #:command-separator #:sequential-sep
   #:wordly-word #:redirect #:fd-description #:condition #:body))
(in-package :shcl/core/shell-grammar)

(optimization-settings)

(define-parser shell-grammar
  (:start-symbol start)
  (:eof-symbol eof)
  (:terminals
   (token a-word simple-word assignment-word name newline io-number and-if
          or-if dsemi dless dgreat lessand greatand lessgreat dlessdash
          clobber if-word then else elif fi do-word done case-word esac while until
          for lbrace rbrace bang in semi par pipe lparen rparen great less))

  (start
   eof
   complete-command)

  (complete-command
   (newline-list complete-command)
   (newline-list)
   (command-list command-end))

  (command-end
   eof
   newline)

  (command-list
   (and-or separator-op command-list-tail)
   and-or)

  (command-list-tail
   (and-or separator-op command-list-tail)
   and-or
   ())

  (and-or
   (pipeline and-or-tail))

  (and-or-tail
   (and-if linebreak pipeline and-or-tail)
   (or-if linebreak pipeline and-or-tail)
   ())

  (pipeline
   (bang pipe-sequence)
   pipe-sequence)

  (pipe-sequence
   (command pipe-sequence-tail))

  (pipe-sequence-tail
   (pipe linebreak command pipe-sequence-tail)
   ())

  (command
   (compound-command redirect-list)
   compound-command
   function-definition
   simple-command)

  (compound-command
   brace-group
   subshell
   for-clause
   case-clause
   if-clause
   while-clause
   until-clause)

  (subshell
   (lparen compound-list rparen))

  (compound-list
   term
   (newline-list term))

  (term
   (and-or separator term-tail)
   and-or)

  (term-tail
   (and-or separator term-tail)
   and-or
   ())

  (for-clause
   (for name-nt linebreak (body do-group))
   (for name-nt linebreak in-nt sequential-sep (body do-group))
   (for name-nt linebreak in-nt wordlist sequential-sep (body do-group)))

  (name-nt
   (name)) ;; Apply rule 5 (need not be reflected in the grammar)

  (in-nt
   (in)) ;; Apply rule 6 (need not be reflected in the grammar)

  (wordlist
   (a-word wordlist-tail))

  (wordlist-tail
   (a-word wordlist-tail)
   ())

  (case-clause
   (case-word a-word linebreak in-nt linebreak case-list esac)
   (case-word a-word linebreak in-nt linebreak case-list-ns esac)
   (case-word a-word linebreak in-nt linebreak esac))

  (case-list-ns
   (case-list case-item-ns)
   (case-item-ns))

  (case-list
   (case-item case-list-tail))

  (case-list-tail
   (case-item case-list-tail)
   ())

  (case-item-ns
   (pattern rparen linebreak)
   (pattern rparen compound-list linebreak)
   (lparen pattern rparen linebreak)
   (lparen pattern rparen compound-list linebreak))

  (case-item
   (pattern rparen linebreak dsemi linebreak)
   (pattern rparen compound-list dsemi linebreak)
   (lparen pattern rparen linebreak dsemi linebreak)
   (lparen pattern rparen compound-list dsemi linebreak))

  (pattern
   (a-word pattern-tail)) ;; Apply rule 4 (must be reflected in grammar)

  (pattern-tail
   (pipe a-word pattern-tail) ;; Do not apply rule 4 (but /bin/sh seems to?)
   ())

  (if-clause
   (if-word (condition compound-list) then (body compound-list) fi)
   (if-word (condition compound-list) then (body compound-list) else-part fi))

  (else-part
   (elif (condition compound-list) then (body compound-list) else-part)
   (elif (condition compound-list) then (body compound-list))
   (else (body compound-list)))

  (while-clause
   (while (condition compound-list) (body do-group)))

  (until-clause
   (until (condition compound-list) (body do-group)))

  (function-definition
   (fname lparen rparen linebreak function-body))

  (function-body
   (compound-command redirect-list) ;; Apply rule 9 (need not be reflected in the grammar)
   (compound-command)) ;; Apply rule 9 (need not be reflected in the grammar)

  (fname
   name) ;; Apply rule 8 (must be reflected in the grammar)

  (brace-group
   (lbrace :strict compound-list rbrace))

  (do-group
   (do-word compound-list done)) ;; Apply rule 6 (need not be reflected in the grammar)

  (simple-command
   (cmd-prefix cmd-word cmd-suffix)
   (cmd-prefix cmd-word)
   (cmd-prefix)
   (cmd-name cmd-suffix)
   (cmd-name))

  (cmd-name
   a-word) ;; Apply rule 7a (might need to be reflected in the grammar)

  (cmd-word
   a-word) ;; Apply rule 7b (might need to be reflected in the grammar)

  (cmd-prefix
   (io-redirect cmd-prefix-tail)
   (assignment-word cmd-prefix-tail))

  (cmd-prefix-tail
   (io-redirect cmd-prefix-tail)
   (assignment-word cmd-prefix-tail)
   ())

  (cmd-suffix
   (io-redirect cmd-suffix-tail)
   (a-word cmd-suffix-tail))

  (cmd-suffix-tail
   (io-redirect cmd-suffix-tail)
   (a-word cmd-suffix-tail)
   ())

  (redirect-list
   (io-redirect redirect-list-tail))

  (redirect-list-tail
   (io-redirect redirect-list-tail)
   ())

  (io-redirect
   io-file
   (io-number (io-source io-file))
   io-here
   (io-number (io-source io-here)))

  (io-file
   ((redirect less) filename)
   ((redirect lessand) (fd-description simple-word))
   ((redirect great) filename)
   ((redirect greatand) (fd-description simple-word))
   ((redirect dgreat) filename)
   ((redirect lessgreat) filename)
   ((redirect clobber) filename))

  (filename
   a-word) ;; Apply rule 2 (need not be reflected in grammar)

  (io-here
   (dless here-end)
   (dlessdash here-end))

  (here-end
   (a-word)) ;; Apply rule 3 (need not be reflected in grammar)

  (newline-list
   (newline newline-list-tail))

  (newline-list-tail
   (newline newline-list-tail)
   ())

  (linebreak
   (newline-list)
   ())

  (separator-op
   par
   semi)

  (separator
   (separator-op linebreak)
   (newline-list))

  (sequential-sep
   (semi linebreak)
   (newline-list)))

(define-advice parse-cmd-name :around posix-rule (iter)
  (when (typep (peek-lookahead-iterator iter) 'reserved-word)
    (return-from parse-cmd-name
      (values
       nil
       (make-internal-parse-error
        :message "Reserved words aren't allowed here"
        :expected-tokens '(a-word)))))
  (call-next-method))

(defun command-iterator (token-iterator)
  (let ((iter (syntax-iterator #'parse-shell-grammar token-iterator)))
    (make-iterator ()
      (do-iterator (value iter)
        (when (eq value 'eof)
          (stop))
        (emit value))
      (stop))))

(defgeneric parse-shell (source))

(defmethod parse-shell ((s string))
  (parse-shell (make-string-input-stream s)))

(defmethod parse-shell ((s stream))
  (parse-shell (lookahead-iterator-wrapper (token-iterator s))))

(defmethod parse-shell ((iter lookahead-iterator))
  (next (command-iterator iter)))

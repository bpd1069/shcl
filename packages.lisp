(defpackage :shcl.utility
  (:use :common-lisp)
  (:shadow #:when-let #:when-let*)
  (:export
   #:define-once-global #:required #:required-argument-missing #:optimization-settings
   #:when-let #:when-let* #:try #:debug-log #:logging-enabled-p #:status
   #:make-extensible-vector
   ;; Hooks
   #:define-hook #:add-hook #:remove-hook #:run-hook #:on-revival
   #:observe-revival #:on-dump #:observe-dump
   ;; Iterators
   #:make-iterator #:emit #:stop #:next #:iterator #:lookahead-iterator
   #:fork-lookahead-iterator #:vector-iterator #:list-iterator #:seq-iterator
   #:do-iterator #:peek-lookahead-iterator #:move-lookahead-to #:map-iterator
   #:iterator-values #:lookahead-iterator-wrapper))

(defpackage :shcl.lexer
  (:use :common-lisp :shcl.utility)
  (:export
   ;; Token classes
   #:token #:a-word #:eof #:simple-word #:compound-word
   #:assignment-word #:name #:io-number #:literal-token #:newline
   #:reserved-word #:single-quote #:double-quote #:command-word
   #:variable-expansion-word

   ;; Slot accessors
   #:token-value #:simple-word-text #:compound-word-parts
   #:assignment-word-name #:assignment-word-value-word #:io-number-fd
   #:literal-token-string #:single-quote-contents #:double-quote-parts
   #:command-word-tokens #:variable-expansion-word-variable

   ;; Operators
   #:and-if #:or-if #:dsemi #:dless #:dgreat #:lessand #:greatand
   #:lessgreat #:dlessdash #:clobber #:semi #:par #:pipe #:lparen
   #:rparen #:great #:less

   ;; Reserved words
   #:if-word #:then #:else #:elif #:fi #:do-word #:done #:case-word #:esac
   #:while #:until #:for #:lbrace #:rbrace #:bang #:in

   ;; Functions
   #:tokenize #:token-iterator #:tokens-in-string #:tokens-in-stream

   ;; Extensible reading
   #:set-character-handler #:make-shell-dispatch-character
   #:set-shell-dispatch-character #:*shell-readtable*
   #:shell-extensible-read #:reset-shell-readtable
   #:lexer-context-mark-end-of-token))

(defpackage :shcl
  (:use :common-lisp :shcl.lexer :shcl.utility)
  (:export #:main #:run-shell-commands-in-stream #:run-shell-commands-in-string))

;;; jml-mode.el --- Major Mode for editing Java Modelling Language

;; Copyright (C) 2019 Alexander Weigl <weigl@kit.edu>
;; URL: https://github.com/wadoon/key-mode
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "24.4") (polymode "20190714.2017"))
;; Package-Version: 0

;;; Usage:
;; Put the following code in your .emacs, site-load.el, or other relevant file
;; (add-to-list 'load-path "path-to-key-mode")
;; (require 'jml-mode)

;;; Commentary:
;; This file provides the support for the rules and problems of the
;; KeY Theorem Prover `https://key-project.org'.
;;
;;
;; It provides following features:
;; 
;; - syntax highlighting 
;; - auto completion of keywords and sorts, functions, and predicates defined in the ldts
;;   via company-mode
;; - planned: flycheck support
;; - planned: snippets

;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:
(require 'polymode)
(require 'jml-contract-mode)
(require 'rx)
;(require 'java-mode)


(defface jml-basic-face
  '((t :background "darkseagreen2")
       :inherit font-lock-comment-face)
  ""
  :group 'jml-faces)
(defvar jml-basic-face 'jml-basic-face)

(defface jml-comment-delimiter-face
  '((t :background "gray"
       :inherit (jml-basic-face font-lock-delimiter-face)))
  ""
  :group 'jml-faces)
(defvar jml-comment-delimiter-face 'jml-comment-delimiter-face)


(defface jml-java-keyword-face
  '((t :inherit (jml-basic-face font-lock-keyword-face)))
  ""
  :group 'jml-faces)
(defvar jml-java-keyword-face  'jml-java-keyword-face)


(defface jml-jml-keyword-face
  '((t ;:background "yellow"
     :underline t
     :inherit (jml-basic-face font-lock-keyword-face)))
  ""
  :group 'jml-faces)
(defvar jml-jml-keyword-face  'jml-jml-keyword-face)


(defface jml-key-keyword-face
  '((t ;:background "purple"
       :inherit (jml-basic-face font-lock-keyword-face)))
  ""
  :group 'jml-faces)
(defvar jml-key-keyword-face  'jml-key-keyword-face)



(defvar jml--java-keywords nil ".")

(defvar jml--jml-keywords nil ".")

(setq jml--java-keywords
      (sort '("boolean" "byte"  "false" "instanceof" "int" "long" "new"
              "public" "private" "protected"
	      "null" "short" "super" "this" "true" "void" ) #'string<))

(setq jml--jml-keywords
      (sort '("accessible" "assignable" "breaks" "continues"
	      "decreases" "depends" "ensures" "ensures_free"
	      "loop_determines" "non_null"
	      "nullable" "represents" "requires" "requires_free"
	      "signals" "signals_only" "\\bigint" "\\duration"
	      "\\elemtype" "\\empty"
	      "\\everything" "\\exception" "\\exists" "\\forall" "\\fresh"
	      "\\index" "\\into" "\\invariant_for" "\\is_initialized" "\\lblneg"
	      "\\lblpos" "\\lockset" "\\max" "\\measured_by" "\\min"
	      "\\nonnullelements" "\\nothing" "\\not_assigned" "\\not_modified"
	      "\\not_specified" "\\num_of" "\\old" "\\permission" "\\pre"
	      "\\product" "\\reach" "\\real" "\\result" "\\same" "\\space"
	      "\\subset" "\\such_that" "\\sum" "\\TYPE" "\\typeof" "\\type"
	      "\\values" "\\working_space")  #'string<))

(defvar jml--key-keywords nil "")
;; Bad keyword "loop_separates"  //KeY extension, deprecated"
(setq jml--key-keywords
      (sort '("\\all_fields"
	      "\\all_objects" "\\backup" "\\bsum" "\\by" "\\declassifies" "\\disjoint"
	      "\\domain_implies_created" "determines"
	      "model_method_axiom" "\\erases" "\\free" "\\seq_indexOf" "\\intersect"
	      "\\inv" "\\in_domain" "\\is_finite" "\\itself" "\\locset"
	      "\\map" "\\map_empty" "\\map_get" "\\map_override"
	      "\\map_remove" "\\map_singleton" "\\map_size" "\\map_update"
	      "\\new_elems_fresh" "\\new_objects" "\\reachLocs" "\\seq" "\\seq_2_map"
	      "\\seq_concat" "\\seq_def" "\\seq_empty" "\\seq_get" "\\seq_put" "\\seq_reverse"
	      "\\seq_singleton" "\\seq_sub" "\\set_minus"
	      "\\singleton" "\\static_invariant_for" "\\strictly_nothing"
	      "\\string_equal" "returns" "join_proc" "separates" "\\set_union" "\\infinite_union"
	      "\\transactionUpdated" "\\transient") #'string<))



(setq jml-prettify-symbols-alist
      '(("lambda" . 955) ;λ
	("<==" . ?← )
	("<==>" . ?↔)
	("<=" . ?≤)
	("==" . ?≡)
	(">=" . ?≥)
	("&&" . ?∧)
	("||" . ?∨)
	("!=" . ?≠)
	(".equals" ?≜)
	("\\forall" . ?∀)
	("\\exists" .?∃)
	("!\\exists" . ?∄)
	("ensures" . ?🔒)
	("requires" . ?🔑)
	("!" . ?¬)))

(defun jml-pretty-symbols ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist jml-prettify-symbols-alist)
  (prettify-symbols-mode 1))

;;font-lock-command-face

(setq jml-font-lock-keywords
      `(
	(,(rx line-start (* (in space)) "@") . jml-comment-delimiter-face)
	(,(regexp-opt jml--java-keywords t) . jml-java-keyword-face)
	(,(regexp-opt jml--jml-keywords t) . jml-jml-keyword-face)
	(,(regexp-opt jml--key-keywords t) . jml-key-keyword-face)
        (,(rx symbol-start ?\\ "dl_" (+ (in alpha alnum)) symbol-end) . jml-key-keyword-face)))



(defvar jml-syntax-table nil "Syntax table for the jml-contract-mode.")
(setq jml-syntax-table
      (let ((table (make-syntax-table java-mode-syntax-table)))
        (modify-syntax-entry ?\" "\"" table)
	(modify-syntax-entry ?\\ "_" table)
	;; / is punctuation, but // is a comment starter
	(modify-syntax-entry ?/ ". 12" table)
	;; \n is a comment ender
	(modify-syntax-entry ?\n ">" table)
 	table))

(define-derived-mode jml-contract-mode fundamental-mode "JML Contract"
  "Editing for JML-Annotations TODO"
  :group 'jml
  :syntax-table jml-syntax-table
  ;;(use-local-map ... )
  (set (make-local-variable 'font-lock-defaults) '(jml-font-lock-keywords)))

(define-hostmode poly-java-hostmode
  :mode 'java-mode)

(define-innermode poly-java-jml-multiline-innermode
  :mode 'jml-contract-mode
  :head-matcher (rx "/*@")
  :tail-matcher (rx "*/")
  :head-mode 'host
  :tail-mode 'host)

(define-innermode poly-java-jml-singleline-innermode
  :mode 'jml-contract-mode
  :head-matcher (rx "//@")
  :tail-matcher (rx eol)
  :head-mode 'host
  :tail-mode 'host)


(define-polymode poly-java-jml-mode
  :hostmode 'poly-java-hostmode
  :innermodes '(poly-java-jml-multiline-innermode
                poly-java-jml-singleline-innermode))



(provide 'jml-mode)

;;; jml-mode.el ends here




(provide 'jml-mode)
;;; jml-mode.el ends here

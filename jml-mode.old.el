;; jml-mode.el -- Minor mode for Java Modeling Language.

;; Copyright (C) 2018 Alexander Weigl

;; Author: Alexander Weigl
;; Version: 0.2 alpha
;; Created: 2018-09-16
;; URL: https://github.com/wadoon/jml-mode

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is a minor mode for Emacs to support JML comments in Java files. It
;; builds ontop of the `java-mode', a derived from the `cc-mode'. In particular,
;; it adds the `jml-font-lock-keywords' that enables you to enable JML comments via
;; the `c-doc-comment-style' variable.

;; For convinience reason, this file provide also the minor mode `jml-mode'.


;; Features
;; * Syntax Highlightning
;; * Face group: *jml*

;;; Todo and Known bugs
;;
;; * [ ] Running/Open File in KeY or OpenJML
;; * [ ] Enabling and disabling not working correctly.
;; * [ ] Prettifying symbols not working
;; * [ ] Snippets. See https://github.com/joaotavora/yasnippet/issues/420 for minor-mode recognition.
;;; Code:

(defface jml-basic-face
  '((t :inherit font-lock-comment-face))
  "This is the base of all jml-faces."
  :group 'jml)
(defvar jml-basic-face 'jml-basic-face)

(defface jml-java-keyword-face
  '((t :inherit (jml-basic-face font-lock-keyword-face)))
  "Face for Java keywords in JML comments."
  :group 'jml)
(defvar jml-java-keyword-face  'jml-java-keyword-face)


(defface jml-jml-keyword-face
  '((t :inherit (jml-basic-face font-lock-keyword-face)))
  "Face for JML keywords in JML comments."
  :group 'jml)
(defvar jml-jml-keyword-face  'jml-jml-keyword-face)


(defface jml-jml-dangerous-face
  '((t
     :underline (line red)
     :inherit (jml-basic-face font-lock-keyword-face)))
  "Face for JML keywords, that can be considered harmful,e.g., ensures_free."
  :group 'jml)
(defvar jml-jml-dangerous-face  'jml-jml-dangerous-face)


;; (defface jml-key-keyword-face
;;   '((t :background "purple"
;;        :inherit (jml-basic-face font-lock-keyword-face)))
;;   ""
;;   :group 'jml)


(defcustom jml-start-multiline (rx (| "/*@" "//@"))
  "Regular expression matching the start of JML comments."
  :group 'jml)

;; (defcustom jml-mode-jml-end-multiline (rx "*/") "" :group 'jml)

(defcustom jml-java-keywords
  '("false" "instanceof"  "new"
    "public" "private" "protected"
    "null"  "super" "this" "true" )
  "List of keywords inside JML contracts, that belongs to Java."
  :type '(repeat string)
  :group 'jml)

(defcustom jml-keywords-types
  (list "boolean" "byte"  "int" "long" "short" "void")
  "Keywords specifying types in Java."
  :type '(repeat string)
  :group 'jml)

(defcustom jml-keywords-1
  (list "ensures" "requires" "accessible" "assignable" "breaks" "continues"
	"decreases" "depends" "ensures" "ensures_free"
	"loop_determines" "non_null" "nullable" "represents" "requires" "requires_free"
	"signals" "signals_only" "maintaining" "decreasing" "assignable"
         "diverges" "normal_behavior" "exceptional_behavior" "pure" "invariant")
  "JML Keywords used in contracts on top level, but are normal identifier on expression level."
  :type '(repeat string) :group 'jml)

(defcustom jml-keywords-2
  (list "bigint" "duration" "elemtype" "empty" "everything" "exception" "exists"
        "forall" "fresh" "index" "into" "invariant_for" "is_initialized" "lblneg"
        "lblpos" "locset" "max" "measured_by" "min"
        "nonnullelements" "nothing" "not_assigned" "not_modified"
        "not_specified" "num_of" "old" "permission" "pre"
        "product" "reach" "real" "result" "same" "space"
        "strictly_nothing" "invariant_for"
        "subset" "such_that" "sum" "TYPE" "typeof" "type"
        "values" "working_space")
  "JML Keywords used in expressions. Prefixed with an \\."
  :type '(repeat string)
  :group 'jml)

(defcustom jml-keywords-dangerous
  (list "ensures_free" "requires_free")
  "JML Keywords that are considered harmful. Highlighted with `jml-jml-dangerous-face'."
  :type '(repeat string)
  :group 'jml)


(defvar jml--font-lock-doc-comments nil
  "Internal variable: containing the keywords for font-lock.")
(setq jml--font-lock-doc-comments
      (let* ((-java-keywords-re (regexp-opt jml-java-keywords 'words))
             (-keywords-1-re (regexp-opt (sort jml-keywords-1 #'string<) 'words))
             (-types-re (regexp-opt (sort jml-keywords-types #'string<) 'words))
             (-danger-re (regexp-opt (sort jml-keywords-dangerous #'string<) 'words))
             (-keywords-2-re (concat (rx ?\\) (regexp-opt (sort jml-keywords-2 #'string<)) (rx eow))))
        `((,-keywords-1-re (0 jml-jml-keyword-face prepend))
          (,-keywords-2-re (0 font-lock-keyword-face prepend))
          (,-types-re (0 font-lock-type-face 'prepend))
          (,-java-keywords-re (0 jml-jml-keyword-face prepend))
          (,-danger-re (0 jml-jml-dangerous-face prepend)))))


(defconst jml-font-lock-keywords
  (list (list
         (lambda (limit)
	   (c-font-lock-doc-comments jml-start-multiline limit
	     jml--font-lock-doc-comments))))
    "Called by cc-mode for jml-comments.")


(defcustom jml-prettify-symbols-alist
  '(("<==" . ?← )
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
    ("!" . ?¬))
  "Prettifying symbols for JML."
  :type '(alist :key-type string :value-type (choice integer character string))
  :group 'jml)

(defun jml-pretty-symbols ()
  "Make some word or string show as pretty Unicode symbols."
  ;;(setq prettify-symbols-alist jml-prettify-symbols-alist)
  (push `("ensures" . ?🔒) prettify-symbols-alist)
  (prettify-symbols-mode 1))


;;;;;
(defcustom jml-command-execute-key "java -jar key.jar '%s'"
  "Command to execute/open a Java file in the KeY theorem prover.

Use %s for the Java filename."
  :type 'string
  :group 'jml)


(defun jml-open-file-in-key ()
  "Opens the current file in the KeY theorem prover."
  (interactive)
  (let ((cmd) (format jml-command-execute-key (buffer-file-name)))
    (shell-command cmd)))


(setq jml-mode-map (let ((map (make-sparse-keymap)))
                  (bind-key (kbd "C-c C-c C-k") #'jml-open-file-in-key)
                  map))


(define-minor-mode jml-mode
  "A minor mode for activating the jml doc style in c-mode.

See `jml-mode-map'.
"
  :ligther " JML"
  :keymap jml-mode-map

  (if jml-mode
      (progn
        (font-lock-add-keywords
         nil
         jml-font-lock-keywords)
        (font-lock-ensure)
        (font-lock-fontify-buffer))
        ;(add-to-list 'c-doc-comment-style '(java-mode . jml))
        ;(c-setup-doc-comment-style)
        ;(java-mode)
        ;;(jml-pretty-symbols))
    (progn
      ;(setq c-doc-comment-style
      ;      (remove '(java-mode . jml) c-doc-comment-style))
      ;;(prettify-symbols-mode -1)
      )))


;; Local Variables:
;; nameless-current-name: "jml"
;; ispell-dictionary: "en_GB"
;; End:

(provide 'jml-mode)
;;; jml-mode.el ends here

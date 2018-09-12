;; jml-mode.el -- Minor mode for Java Modeling Language.

;;; Commentary:

;;; Code:

(defface jml-basic-face
  '((t :background "darkseagreen2")
       :inherit font-lock-comment-face)
  ""
  :group 'jml)
(defvar jml-basic-face 'jml-basic-face)


(defface jml-java-keyword-face
  '((t :background "red"
       :inherit (jml-basic-face font-lock-keyword-face)))
  ""
  :group 'jml)
(defvar jml-java-keyword-face  'jml-java-keyword-face)


(defface jml-jml-keyword-face
  '((t :background "yellow"
       :inherit (jml-basic-face font-lock-keyword-face)))
  ""
  :group 'jml)
(defvar jml-jml-keyword-face  'jml-jml-keyword-face)


(defface jml-key-keyword-face
  '((t :background "purple"
       :inherit (jml-basic-face font-lock-keyword-face)))
  ""
  :group 'jml)


(defcustom jml-start-multiline (rx (| "/*@" "//@"))
  "" :group 'jml)

;; (defcustom jml-mode-jml-end-multiline (rx "*/") "" :group 'jml)

(defcustom jml-java-keywords
  '("false" "instanceof"  "new"
    "public" "private" "protected"
    "null"  "super" "this" "true" )
  "List of keywords inside JML contracts, that belongs to Java"
  :type '(list string)
  :group 'jml)

(defcustom jml-keywords-types
  (list "boolean" "byte"  "int" "long" "short" "void")
  "."
  :type '(list string)
  :group 'jml)



(defcustom jml-keywords-1
  (list "ensures" "requires" "accessible" "assignable" "breaks" "continues"
	"decreases" "depends" "ensures" "ensures_free"
	"loop_determines" "non_null" "nullable" "represents" "requires" "requires_free"
	"signals" "signals_only" "maintaining" "decreasing" "assignable"
         "diverges" "normal_behavior" "exceptional_behavior" "pure" "invariant")
  "JML Keywords used in contracts on top level, but are normal identifier on expression level."
  :type '(list string) :group 'jml)

(defcustom jml-keywords-2
  (list
   "bigint" "duration" "elemtype" "empty" "everything" "exception" "exists"
   "forall" "fresh" "index" "into" "invariant_for" "is_initialized" "lblneg"
   "lblpos" "locset" "max" "measured_by" "min"
   "nonnullelements" "nothing" "not_assigned" "not_modified"
   "not_specified" "num_of" "old" "permission" "pre"
   "product" "reach" "real" "result" "same" "space"
   "strictly_nothing" "invariant_for"
   "subset" "such_that" "sum" "TYPE" "typeof" "type"
   "values" "working_space")
  "JML Keywords used in expressions. Prefixed with an \\."
  :type '(list string)
  :group 'jml)

(defvar jml--font-lock-doc-comments nil
  "Keywords for font-lock")

(setq jml--font-lock-doc-comments
      (let* ((-java-keywords-re (regexp-opt jml-java-keywords 'words))
             (-keywords-1-re (regexp-opt (sort jml-keywords-1 #'string<) 'words))
             (-types-re (regexp-opt (sort jml-keywords-types #'string<) 'words))
             ;;(-keywords-2-re (rx-to-string `(: bow "\\" (| ,@jml-keywords-2) eow))))
             (-keywords-2-re (concat (rx ?\\) (regexp-opt (sort jml-keywords-2 #'string<)))))
        `((,-keywords-1-re (0 font-lock-keyword-face prepend))
          (,-keywords-2-re (0 font-lock-keyword-face prepend))
          (,-types-re (0 font-lock-type-face 'prepend))
          (,-java-keywords-re (0 font-lock-keyword-face prepend)))))

(defun jml-font-lock-keywords ()
  "Get calls by cc-mode for jml-comments."
  `((,(lambda (limit)
	(c-font-lock-doc-comments jml-start-multiline limit
	  jml--font-lock-doc-comments)))))


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
  :type '(const string . string)
  :group 'jml)

(defun jml-pretty-symbols ()
  "make some word or string show as pretty Unicode symbols"
                                        ;(setq prettify-symbols-alist jml-prettify-symbols-alist)
  (push `("ensures" . ?🔒) prettify-symbols-alist)
  (prettify-symbols-mode 1))


(define-minor-mode jml-mode
  "A minor mode for activating the jml doc style in c-mode"
  :ligther " JML"

  (if jml-mode
      (progn
        (add-to-list 'c-doc-comment-style '(java-mode . jml))
        (c-setup-doc-comment-style)
        (java-mode)
        (jml-pretty-symbols))
    (progn
      (setq c-doc-comment-style
            (remove '(java-mode . jml) c-doc-comment-style))
      (prettify-symbols-mode -1))))

;; Local Variables:
;; nameless-current-name: "jml"
;; End:

(provide 'jml-mode)
;;; jml-mode.el ends here

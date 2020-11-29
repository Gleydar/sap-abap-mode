;; Keywords: SAP ABAP CDS Mode, Emacs

;; TODOs
;; - maybe rename abap-cds-mode-hook
;; - maybe rename abap-cds-mode

(require 'abap-mode)

(defvar abap-cds-mode-hook nil)

(defcustom abap-cds-indent-level 4
  "Indentation of ABAP CDS with respect to containing block."
  :type 'integer)

(defun abap-cds-is-empty-line()
  "Check whether line is empty, whitespaces and TABs are not significant."
  ;; (beginning-of-line)
  (save-excursion
    ;; (back-to-indentation)
    ;; (looking-at "$")))
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun abap-cds-in-comment-p()
  "Check whether cursor is in comment block."
  (nth 4 (syntax-ppss)))

(defun abap-cds-indent-line()
  "Indent ABAP CDS Line"
  (interactive)
  (let ((cur-indent 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "[{]")
              (setq cur-indent (+ cur-indent abap-cds-indent-level))))
        (error nil))) ; end save-excursion
    (save-excursion
      (back-to-indentation)
      (cond ((abap-cds-in-comment-p) ; check for multiline comment block
             (setq cur-indent (+ cur-indent 1)))
            ((and (looking-at "[}]") (>= cur-indent abap-cds-indent-level))
             (setq cur-indent (- cur-indent abap-cds-indent-level)))))
    (indent-line-to cur-indent)))

(setq abap-cds-keywords
      '(
        "DEFINE" "VIEW" "SELECT" "AS" "FROM" "ASSOCIATION" "PROJECTION" "ON" "WHERE"
        "KEY" "REDIRECTED" "TO" "LOCALIZED" "ENTITY" "ROOT" "PARENT" "COMPOSITION CHILD" "EXTEND"
        ))
(setq abap-cds-keywords (append abap-cds-keywords (mapcar 'downcase abap-cds-keywords)))

(setq abap-cds-keywords-regexp (regexp-opt abap-cds-keywords 'symbols))
(setq abap-cds-font-lock-keywords
      `(
        ("^@.*$" . font-lock-builtin-face)
        (,abap-cds-keywords-regexp . font-lock-keyword-face)
        ))

(defvar abap-cds-mode-syntax-table
  (let ((abap-cds-mode-syntax-table (make-syntax-table)))
    ;; set \" back to non-comment syntax TODO maybe remove
    (modify-syntax-entry ?\" "w")
    (modify-syntax-entry ?' "\"")
    (modify-syntax-entry ?_  "w")
    ;; comments in ABAP CDS
    (modify-syntax-entry ?\/ ". 124b")
    (modify-syntax-entry ?* ". 23")
    (modify-syntax-entry ?\n "> b")
    abap-cds-mode-syntax-table)
  "Syntax table for ABAP CDS Mode")

;;;###autoload
(define-derived-mode abap-cds-mode prog-mode
  "ABAP CDS Mode"
  ;; Major mode for ABAP Core Data Services
  (set-syntax-table abap-cds-mode-syntax-table)

  ;;; Search Based
  ;; Code for syntax highlighting
  (setq-local font-lock-defaults '(abap-cds-font-lock-keywords nil nil))
  (setq-local indent-line-function 'abap-cds-indent-line)

  (run-hooks 'abap-cds-mode-hook)
  )

;; clear memory
(setq abap-cds-keywords nil)
(setq abap-cds-keywords-regexp nil)

;; add the mode to the list
(provide 'abap-cds-mode)

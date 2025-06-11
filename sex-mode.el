(require 'scheme)

(defgroup sex-mode nil
  "Major mode for Sex code."
  :prefix 'sex-
  :group 'languages)

(define-abbrev-table 'sex-mode-abbrev-table ()
  "Abbrev table for Sex mode.
It has `scheme-mode-abbrev-table' as its parent."
  :parents (list scheme-mode-abbrev-table))

(defvar sex-mode-syntax-table
  (let ((table (make-syntax-table lisp-data-mode-syntax-table)))
    table))

(defvar sex-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    map)
  "Keymap for Sex mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

(defvar sex-mode-line-process "")

(defconst sex-font-lock-keywords
  (eval-when-compile
    (list
     ;; Declarations
     (list (concat "("
                   (regexp-opt '("include"
                                 "fn"
                                 "pub"
                                 "struct"
                                 "template"
                                 "var")
                               'word)
                   "\\>"
                   "[[:space:]]*"
                   "\\([[:word:]]*\\)")
           '(1 font-lock-keyword-face)
           '(2 font-lock-function-name-face))
     ;; Keywords
     (list (concat "("
                   (regexp-opt '(
                                 "case"
                                 "do"
                                 "if"
                                 "for"
                                 "goto"
                                 "return"
                                 "switch"
                                 "var"
                                 "while")
                               'word)
                   "\\>")
           '(1 'font-lock-builtin-face)))))

(defun sex-mode-set-variables ()
  (set-syntax-table sex-mode-syntax-table)
  (setq local-abbrev-table sex-mode-abbrev-table)
  (setq mode-line-process '("" sex-mode-line-process))
  (setq font-lock-defaults
        '((sex-font-lock-keywords)
          nil nil
          (("+-*/.<>=!?$%_&:" . "w"))
          nil
          (font-lock-mark-block-function . mark-defun)))
  (setq-local prettify-symbols-alist lisp-prettify-symbols-alist))

(put 'fn 'lisp-indent-function 'defun)
(put 'template 'lisp-indent-function 'defun)
(put 'struct 'lisp-indent-function 'defun)
(put 'var 'lisp-indent-function 0)

;;;###autoload
(define-derived-mode sex-mode lisp-data-mode "Sex"
  "Major mode for editing Sex code.
Editing commands are similar to those of `lisp-mode'.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{sex-mode-map}"
  :group 'sex-mode
  (sex-mode-set-variables))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(sex\\|hsex\\)\\'" . sex-mode))

(provide 'sex-mode)

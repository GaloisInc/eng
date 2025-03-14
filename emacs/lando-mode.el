;; -*- mode: Lisp; lexical-binding: t; -*-

(defvar lando-mode-hook nil)

(defvar lando-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Lando major mode")

(defvar lando-keywords
  '("system" "subsystem" "contains" "end" "import" "inherit" "client" "indexing"
    "component" "events" "scenarios" "requirements" "requirement"
    ))

(defvar lando-types
  '("boolean" "integer" "float"
    ))

(defvar lando-constants
  '("shall" "satisfy"
    ))

(defvar lando-builtins
  '( "." "?" "!"
     "FRET" "Input" "Output" "Mode" "Internal" "Function"
    ))

(defvar lando-font-lock-defaults
  `(
    ( ,(regexp-opt lando-keywords 'words) . font-lock-keyword-face)
    ( ,(regexp-opt lando-types 'words) . font-lock-type-face)
    ( ,(regexp-opt lando-constants 'words) . font-lock-constant-face)
    ( ,(regexp-opt lando-builtins 'symbols) . font-lock-builtin-face)
    ))

(defvar lando-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; start of 2-char comment seq (/*), may also be second char of a two char
    ;; comment sequence (//), and may be the end of a 2-char comment seq (*/)
    (modify-syntax-entry ?/ ". 12b" table)
    ;; newline ends a b-style comment
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for lando mode.")

(define-derived-mode lando-mode prog-mode "lando"
  "Major mode for editing Lando specification files"
  :group 'lando-mode
  :syntax-table lando-mode-syntax-table
  :abbrev-table (make-abbrev-table)

  (setq font-lock-defaults '(lando-font-lock-defaults))

  ;; Indenting of comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  ; (setq-local comment-start-skip "\\(^\\|\\s-\\);?#+  ")
  (setq-local comment-multi-line t)

  ;; Filling of comments
  (setq-local adaptive-fill-mode t)
  (setq-local paragraph-start "[ \t]*\\(//+\\|\\**\\)[ \t]*$\\|^^L")
  (setq-local paragraph-separate paragraph-start)
  )

(add-to-list 'auto-mode-alist '("\\.lando\\'" . lando-mode))

(provide 'lando-mode)

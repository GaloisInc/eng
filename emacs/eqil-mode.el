;; -*- mode: Lisp; lexical-binding: t; -*-

(defvar eqil-mode-hook nil)

(defvar eqil-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for EQIL major mode")

;; (defvar eqil-keywords
;;   '( ;; key = ...
;;     ("^[ \t]*\\(.+?\\)[ \t]*=" (1 'font-lock-variable-name-face))
;;     ;; key
;;     ("^[ \t]*\\(.+?\\)[ \t]$"  (1 'font-lock-variable-name-face))
;;     )
;;   "Keywords to highlight in EQIL mode")
;;                         ))
;; (defvar eqil-types '())
;; (defvar eqil-constants '())
;; (defvar eqil-builtins '("="))

;; (defvar eqil-font-lock-defaults
;;   `(
;;     ( ,(regexp-opt eqil-keywords 'words) . font-lock-keyword-face)
;;     ( ,(regexp-opt eqil-types 'words) . font-lock-type-face)
;;     ( ,(regexp-opt eqil-constants 'words) . font-lock-constant-face)
;;     ( ,(regexp-opt eqil-builtins 'symbols) . font-lock-builtin-face)
;;     ))

;; (defvar eqil-mode-syntax-table
;;   (let ((table (make-syntax-table)))
;;     (modify-syntax-entry ?= "." table)
;;     table)
;;   "Syntax table for eqil mode.")

(define-derived-mode eqil-mode conf-mode "EQIL"
  "Major mode for editing EQIL specification files"
  :group 'eqil-mode
  ;; :syntax-table lando-mode-syntax-table
  ;; :abbrev-table (make-abbrev-table)

  ;; (setq font-lock-defaults '(lando-font-lock-defaults))

  )

(add-to-list 'auto-mode-alist '("\\.eng\\'" . eqil-mode))

(provide 'eqil-mode)

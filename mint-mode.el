;;; mint-mode.el --- major mode for editing .mint files. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Diwank Tomer ( singh@diwank.name )
;; Version: 0.1.0
;; Created: 14 Nov 2018
;; Homepage: https://github.com/creatorrr/emacs-mint-mode

;; Major mode for writing programs in mint lang.
;; https://mint-lang.com

(require 'generic-x)

(define-generic-mode
    ;; name
  'mint-mode

  ;; comment delimiter
  '('("/*" "*/"))

  ;; keywords list
  '("module" "record" "routes" "provider"
    "store" "state" "fun" "use" "next"
    "component" "style" "property" "get" "connect" "exposing"
    "decode" "encode"
    "if" "else" "case" "try" "catch"
    "do" "sequence" "parallel")

  ;; font lock list
  '(("=" . 'font-lock-operator)
    ("::" . 'font-lock-builtin))

  ;; file extensions to watch
  '("\\.mint$")

  ;; other functions to call
  nil

  ;; description
  "Major mode for writing programs in mint lang.")


;; (setq mint-font-lock-keywords
;;       (let* (
;;             ;; define several category of keywords
;;             (x-keywords '("break" "default" "do" "else" "for" "if" "return" "state" "while"))
;;             (x-types '("float" "integer" "key" "list" "rotation" "string" "vector"))
;;             (x-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
;;             (x-events '("at_rot_target" "at_target" "attach"))
;;             (x-functions '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList"))
;;
;;             ;; generate regex string for each category of keywords
;;             (x-keywords-regexp (regexp-opt x-keywords 'words))
;;             (x-types-regexp (regexp-opt x-types 'words))
;;             (x-constants-regexp (regexp-opt x-constants 'words))
;;             (x-events-regexp (regexp-opt x-events 'words))
;;             (x-functions-regexp (regexp-opt x-functions 'words)))
;;
;;         `(
;;           (,x-types-regexp . font-lock-type-face)
;;           (,x-constants-regexp . font-lock-constant-face)
;;           (,x-events-regexp . font-lock-builtin-face)
;;           (,x-functions-regexp . font-lock-function-name-face)
;;           (,x-keywords-regexp . font-lock-keyword-face)
;;           ;; note: order above matters, because once colored, that part won't change.
;;           ;; in general, put longer words first
;;           )))
;;
;; ;;;###autoload
;; (define-derived-mode mylsl-mode c-mode "lsl mode"
;;   "Major mode for editing LSL (Linden Scripting Language)…"
;;
;;   ;; code for syntax highlighting
;;   (setq font-lock-defaults '((mylsl-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'mint-mode)

;;; mint-mode.el ends here

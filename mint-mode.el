;;; mint-mode.el --- major mode for editing .mint files. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Diwank Tomer ( singh@diwank.name )
;; Summary: Major mode for editing .mint files.
;; Version: 0.5.0
;; Homepage: https://github.com/creatorrr/emacs-mint-mode
;; URL: https://github.com/creatorrr/emacs-mint-mode
;; Created: 18 Nov 2018
;; Keywords: mint languages processes convenience tools files
;; Package-Requires: ((emacs "24.4"))

;;; License:

;; mint-mode.el --- major mode for editing .mint files.
;; Copyright (C) 2018 Diwank Tomer <github.com/creatorrr>
;;
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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Major mode for writing programs in mint lang. Provides:
;; - Syntax highlighting
;; - Auto format on save using `mint format`
;;
;; For more info on mint, visit: https://mint-lang.com

;;; Code:
(eval-when-compile
  (require 'js)
  (require 'seq)
  (require 'subr-x))

;; Utils
(defun mint-get-tokens (filename)
  "Get tokens defined from data file FILENAME."

  (let* ((script-dir (file-name-directory load-file-name))
         (filepath (expand-file-name filename script-dir))
         (contents (with-temp-buffer
                     (insert-file-contents-literally filepath)
                     (buffer-string)) )

         (raw-tokens (split-string contents "\n"))
         (trimmed-tokens (mapcar 'string-trim raw-tokens))

         (string-not-empty-p (lambda (str)
                               (not (string-empty-p str))) ))

    ;; Return list minus empty lines
    (seq-filter string-not-empty-p trimmed-tokens) ))

;; For highlighting language tokens
;; Simple
(defvar mint-lang-blocks (mint-get-tokens "./tokens/lang/blocks.txt"))
(defvar mint-lang-declarators (mint-get-tokens "./tokens/lang/declarators.txt"))
(defvar mint-lang-initializers (mint-get-tokens "./tokens/lang/initializers.txt"))
(defvar mint-lang-keywords (mint-get-tokens "./tokens/lang/keywords.txt"))
(defvar mint-lang-specifiers (mint-get-tokens "./tokens/lang/specifiers.txt"))
(defvar mint-lang-literal-types (mint-get-tokens "./tokens/lang/literal-types.txt"))

;; Compound
(defvar mint-lang-compound-types (mint-get-tokens "./tokens/lang/compound-types.txt"))
(defvar mint-lang-operators (mint-get-tokens "./tokens/lang/operators.txt"))

;; For highlighting html tags
(defvar mint-html-tags (mint-get-tokens "./tokens/html/tags.txt"))

;; For highlighting css tokens
(defvar mint-style-colors (mint-get-tokens "./tokens/style/colors.txt"))
(defvar mint-style-properties (mint-get-tokens "./tokens/style/properties.txt"))
(defvar mint-style-units (mint-get-tokens "./tokens/style/units.txt"))

;; All combined
(defvar mint-all-style-tokens (append mint-style-units mint-style-properties mint-style-colors))
(defvar mint-all-lang-tokens (append
                              mint-lang-operators mint-lang-compound-types mint-lang-literal-types
                              mint-lang-specifiers mint-lang-keywords mint-lang-initializers
                              mint-lang-declarators mint-lang-blocks))

(defvar mint-all-tokens (append mint-all-lang-tokens mint-all-style-tokens mint-html-tags))

;; Define regular expressions for syntax highlighting
(setq mint-font-lock-keywords

         ;; For simple keywords like `do`, `fun` etc.
  (let* ((regex-blocks (regexp-opt mint-lang-blocks 'words))
         (regex-declarators (regexp-opt mint-lang-declarators 'words))
         (regex-initializers (regexp-opt mint-lang-initializers 'words))
         (regex-keywords (regexp-opt mint-lang-keywords 'words))
         (regex-specifiers (regexp-opt mint-lang-specifiers 'words))
         (regex-literal-types (regexp-opt mint-lang-literal-types 'words))

         ;; For compound type constructors like `Maybe(Number)`
         (regex-compound-type-constructors
          (mapconcat (lambda (type)
                       (concat (regexp-quote type) "[[:space:]]*" "("))

                     mint-lang-compound-types
                     "\\|") )


         ;; For compound type classes like `Maybe.just`
         (regex-compound-type-classes
          (mapconcat (lambda (type)
                       (concat (regexp-quote type) "\\."))

                     mint-lang-compound-types
                     "\\|") )

         ;; For operators like `=>`
         (regex-operators
          (mapconcat (lambda (type)
                       (concat "[[:space:]]+" (regexp-quote type) "[[:space:]]*"))

                     mint-lang-operators
                     "\\|") )

         ;; For html tag-open (no style applied)
         (regex-html-tag-open
          (mapconcat (lambda (type)
                       (concat "<" "[[:space:]]*" (regexp-quote type) "[[:space:]]*" ">"))
                     mint-html-tags
                     "\\|") )

         (regex-html-tag-open-with-attr
          (mapconcat (lambda (type)
                       (concat "<" "[[:space:]]*" (regexp-quote type) "[[:space:]]+" "[a-zA-Z\\-]+" "[[:space:]]*" "="))
                     mint-html-tags
                     "\\|") )

         ;; For html tag-open (style applied)
         (regex-html-tag-open-with-style
          (mapconcat (lambda (type)
                       (concat "<" "[[:space:]]*" (regexp-quote type) "[[:space:]]*" "::"))
                     mint-html-tags
                     "\\|") )

         ;; For html tag-close
         (regex-html-tag-close
          (mapconcat (lambda (type)
                       (concat "<" "/" "[[:space:]]*" (regexp-quote type) "[[:space:]]*" ">"))
                     mint-html-tags
                     "\\|") )

         ;; ;; For style colors
         (regex-style-colors (regexp-opt mint-style-colors 'words))

         ;; For style property names
         (regex-style-properties
          (mapconcat (lambda (type)
                       (concat (regexp-quote type) "[[:space:]]*" ":"))

                     mint-style-properties
                     "\\|") )

         ;; For style units
         (regex-style-units
          (mapconcat (lambda (type)
                       (concat "[[:digit:]]+" "[[:space:]]*" (regexp-quote type)))

                     mint-style-units
                     "\\|") )

         ;; Other misc categories
         (regex-inline-marker "`"))

    ;; Set font-lock mode face for each category
    `((,regex-blocks . font-lock-constant-face)
      (,regex-declarators . font-lock-constant-face)
      (,regex-initializers . font-lock-type-face)
      (,regex-keywords . font-lock-warning-face)
      (,regex-specifiers . font-lock-builtin-face)
      (,regex-literal-types . font-lock-variable-name-face)

      (,regex-compound-type-constructors . font-lock-type-face)
      (,regex-compound-type-classes . font-lock-string-face)
      (,regex-operators . font-lock-variable-name-face)

      (,regex-html-tag-open . font-lock-variable-name-face)
      (,regex-html-tag-open-with-attr . font-lock-variable-name-face)
      (,regex-html-tag-open-with-style . font-lock-variable-name-face)
      (,regex-html-tag-close . font-lock-variable-name-face)

      (,regex-style-colors . font-lock-constant-face)
      (,regex-style-properties . font-lock-variable-name-face)
      (,regex-style-units . font-lock-builtin-face)

      (,regex-inline-marker . font-lock-warning-face) )))

;; Auto complete at point table
(defun mint-keyword-completion-at-point ()
  "Provide completion at point table to company-mode."

  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))

    (when bounds
      (list (car bounds)
            (cdr bounds)

            mint-all-tokens

            :exclusive 'no
            :company-docsig #'identity
            :company-doc-buffer (lambda (cand)
                                  (company-doc-buffer (format "'%s' is defined in mint-mode plugin" cand))) )) ))

;; Function for reformatting .mint source files
(defun mint-format-file ()
  "Formats current file using `mint format`."

  (let* ((file buffer-file-name)
         (error-file (make-temp-file "mint-format-errors-file"))
         (command (concat "mint format " file " > " error-file))

         ;; Error container
         (error-buffer (get-buffer-create "*prettier errors*"))

         ;; Revert options
         (ignore-auto t)
         (noconfirm t)
         (preserve-modes t)

         ;; Run command in process
         (result (call-process-shell-command command nil nil nil)) )

    ;; Check command result
    (if (zerop result)

      ;; Update formatted file and destroy error-buffer
      (progn
        (kill-buffer error-buffer)
        (revert-buffer ignore-auto noconfirm preserve-modes))

      ;; Show errors
      (progn
        (with-current-buffer error-buffer
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert-file-contents error-file t nil nil)
          (ansi-color-apply-on-region (point-min) (point-max))
          (compilation-mode))

        (display-buffer error-buffer)) )

    ;; Remove temporary error file
    (delete-file error-file) ))

;;;###autoload
(define-derived-mode mint-mode js-jsx-mode "mint mode"
  "Major mode for writing programs in mint lang."

  ;; Register auto complete fn
  (push 'mint-keyword-completion-at-point completion-at-point-functions)

  ;; hook for formatting on save
  (add-hook 'mint-mode-hook (lambda () (add-hook 'after-save-hook #'mint-format-file nil 'local)))

  ;; For correctly formatting ansi terminal color codes
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

  ;; code for syntax highlighting
  (setq font-lock-defaults '((mint-font-lock-keywords))))

(add-to-list 'auto-mode-alist '("\\.mint\\'" . mint-mode))

;; add the mode to the `features' list
(provide 'mint-mode)

;;; mint-mode.el ends here

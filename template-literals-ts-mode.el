;;; template-literals-ts-mode.el --- Tree-sitter support for HTML/CSS in JS/TS template literals -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Ian S. Pringle
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, javascript, typescript, tree-sitter
;; URL: https://github.com/ispringle/template-literals-ts-mode
;; License: 0BSD

;;; Commentary:

;; This package provides tree-sitter based syntax highlighting and
;; indentation for HTML and CSS inside JavaScript and TypeScript tagged
;; template literals (e.g., html`...` and css`...` as used by Lit and others).
;;
;; Requires Emacs 30+ with tree-sitter support and the javascript,
;; typescript, css, and html grammars installed.
;;
;; Usage:
;;   (add-hook 'js-ts-mode-hook #'template-literals-ts-mode)
;;   (add-hook 'typescript-ts-mode-hook #'template-literals-ts-mode)

;;; Code:

(require 'treesit)
(require 'js)
(require 'css-mode)
(require 'html-ts-mode)

(defgroup template-literals-ts nil
  "Tree-sitter support for HTML/CSS in JS/TS template literals."
  :group 'languages
  :prefix "template-literals-ts-")

(defvar-local template-literals-ts--host-language nil
  "The host language for the current buffer (javascript or typescript).")

(defun template-literals-ts--detect-host-language ()
  "Detect whether we're in a JavaScript or TypeScript buffer."
  (if (seq-find (lambda (p) (eq (treesit-parser-language p) 'typescript))
                (treesit-parser-list))
      'typescript
    'javascript))

(defun template-literals-ts--language-at-point (point)
  "Return the language at POINT for tagged template literals."
  (let ((css-parser (seq-find (lambda (p) (eq (treesit-parser-language p)
                                              'css))
                              (treesit-parser-list)))
        (html-parser (seq-find (lambda (p) (eq (treesit-parser-language p)
                                               'html))
                               (treesit-parser-list))))
    (cond
     ((and css-parser
           (seq-some (lambda (range)
                       (and (<= (car range) point)
                            (< point (cdr range))))
                     (treesit-parser-included-ranges css-parser)))
      'css)
     ((and html-parser
           (seq-some (lambda (range)
                       (and (<= (car range) point)
                            (< point (cdr range))))
                     (treesit-parser-included-ranges html-parser)))
      'html)
     (t template-literals-ts--host-language))))

(defun template-literals-ts--template-base-column ()
  "Get the base indentation column for the current template string."
  (save-excursion
    (let ((host-parser (seq-find (lambda (p)
                                   (eq (treesit-parser-language p)
                                       template-literals-ts--host-language))
                                 (treesit-parser-list))))
      (when-let* ((node (treesit-node-at (point) host-parser))
                  (template (treesit-parent-until
                             node
                             (lambda (n)
                               (equal (treesit-node-type n)
                                      "template_string")))))
        (goto-char (treesit-node-start template))
        (back-to-indentation)
        (+ (current-column)
           (pcase template-literals-ts--host-language
             ('typescript typescript-ts-mode-indent-offset)
             (_ js-indent-level)))))))

(defun template-literals-ts--in-template-p ()
  "Return t if point is inside template string content, nil if on opening line."
  (let ((host-parser (seq-find (lambda (p)
                                 (eq (treesit-parser-language p)
                                     template-literals-ts--host-language))
                               (treesit-parser-list))))
    (when-let* ((node (treesit-node-at (point) host-parser))
                (template (treesit-parent-until
                           node
                           (lambda (n)
                             (equal (treesit-node-type n) "template_string")))))
      (> (line-number-at-pos (point))
         (line-number-at-pos (treesit-node-start template))))))

(defun template-literals-ts--calculate-indent ()
  "Calculate indentation for CSS/HTML inside template literals."
  (let* ((lang (treesit-language-at (point)))
         (indent-unit (pcase lang
                        ('css css-indent-offset)
                        ('html sgml-basic-offset)
                        (_ 2)))
         (parser (seq-find (lambda (p) (eq (treesit-parser-language p) lang))
                           (treesit-parser-list)))
         (node (save-excursion
                 (back-to-indentation)
                 (if (eolp)
                     (progn
                       (forward-line -1)
                       (end-of-line)
                       (treesit-node-at (point) parser))
                   (treesit-node-at (point) parser))))
         (node-type (treesit-node-type node))
         (parent (treesit-node-parent node))
         (depth 0)
         (in-rule-set nil)
         (found-block nil))
    (while parent
      (let ((type (treesit-node-type parent))
            (grandparent (treesit-node-parent parent)))
        (when (equal type "rule_set")
          (setq in-rule-set t))
        (when (member type (pcase lang
                             ('css '("block"))
                             ('html '("element"))))
          ;; For HTML: skip rnoot element only for tag nodes, not content
          (unless (and (eq lang 'html)
                       (equal type "element")
                       (equal (treesit-node-type grandparent) "document")
                       (member node-type '("<" ">" "</" "/>" "tag_name")))
            (setq depth (1+ depth))
            (setq found-block t))))
      (setq parent (treesit-node-parent parent)))
    (when (and (eq lang 'css)
               in-rule-set
               (not found-block)
               (not (member node-type
                            '("class_selector" "id_selector" "tag_name"
                              "." "#" "selectors" "class_name"))))
      (setq depth 1))
    (when (and (eq lang 'css)
               (equal node-type "}"))
      (setq depth (1- depth)))
    (* indent-unit depth)))

(defun template-literals-ts--indent-line ()
  "Indent line for template literals with embedded CSS/HTML."
  (treesit-update-ranges)
  (let ((lang (treesit-language-at (point))))
    (if (and (memq lang '(css html))
             (template-literals-ts--in-template-p))
        (let* ((base-col (or (template-literals-ts--template-base-column) 0))
               (inner-indent (template-literals-ts--calculate-indent))
               (target (+ base-col inner-indent))
               (offset (- (current-column) (current-indentation))))
          (indent-line-to target)
          (when (> offset 0)
            (forward-char offset)))
      (treesit-indent))))

(defun template-literals-ts--setup ()
  "Set up tree-sitter parsers and font-lock for template literals."
  (setq template-literals-ts--host-language
        (template-literals-ts--detect-host-language))
  
  (treesit-parser-create 'css)
  (treesit-parser-create 'html)
  
  (setq-local treesit-range-settings
              (treesit-range-rules
               :embed 'css
               :host template-literals-ts--host-language
               '((call_expression
                  function: (identifier) @_tag
                  arguments: (template_string (string_fragment) @capture)
                  (:equal @_tag "css")))
               :embed 'html
               :host template-literals-ts--host-language
               '((call_expression
                  function: (identifier) @_tag
                  arguments: (template_string (string_fragment) @capture)
                  (:equal @_tag "html")))))
  
  (treesit-update-ranges)
  (setq-local treesit-language-at-point-function
              #'template-literals-ts--language-at-point)
  (setq-local indent-line-function #'template-literals-ts--indent-line)
  
  (setq-local treesit-font-lock-settings
              (append treesit-font-lock-settings
                      (treesit-font-lock-rules
                       :language 'css
                       :feature 'selector
                       :override t
                       '((class_selector (class_name) @font-lock-type-face)
                         (id_selector (id_name) @font-lock-keyword-face)
                         (tag_name) @font-lock-function-name-face)
                       :language 'css
                       :feature 'property
                       :override t
                       '((property_name) @font-lock-property-name-face
                         (plain_value) @font-lock-constant-face
                         (color_value) @font-lock-constant-face
                         (integer_value) @font-lock-number-face
                         (float_value) @font-lock-number-face)
                       
                       :language 'html
                       :feature 'tag
                       :override t
                       '((tag_name) @font-lock-function-name-face
                         (attribute_name) @font-lock-property-name-face
                         (quoted_attribute_value) @font-lock-string-face))))
  
  (font-lock-flush))

;;;###autoload
(define-minor-mode template-literals-ts-mode
  "Minor mode for HTML/CSS syntax support in JS/TS template literals.

Provides tree-sitter based syntax highlighting and indentation for
tagged template literals using `html` and `css` tags."
  :lighter " TplLit"
  :group 'template-literals-ts
  (if template-literals-ts-mode
      (template-literals-ts--setup)
    (message
     "Disabling template-literals-ts-mode requires reopening the buffer")))

(provide 'template-literals-ts-mode)

;;; template-literals-ts-mode.el ends here

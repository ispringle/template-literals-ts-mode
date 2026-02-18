;;; template-literals-ts-mode.el --- Tree-sitter support for HTML/CSS in JS/TS template literals -*- lexical-binding: t; -*-

;; Author: Ian S. Pringle <ian@dapringles.org>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, javascript, typescript, tree-sitter
;; URL: https://github.com/ispringle/template-literals-ts-mode
;; License: 0BSD
;; SPDX-License-Identifier: 0BSD

;; Copyright (C) 2026 by Ian S. Pringle ian@dapringles.com
;; 
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; This package provides tree-sitter based syntax highlighting and
;; indentation for embedded languages inside JavaScript and TypeScript
;; tagged template literals (e.g., html`...` and css`...` as used by
;; Lit and others).
;;
;; Requires Emacs 30+ with tree-sitter support and the javascript,
;; typescript grammars installed, plus grammars for each embedded
;; language you configure.
;;
;; Usage:
;;   (add-hook 'js-ts-mode-hook #'template-literals-ts-mode)
;;   (add-hook 'typescript-ts-mode-hook #'template-literals-ts-mode)
;;
;; By default, html`...` and css`...` tagged literals are supported.
;; Customize `template-literals-ts-tag-grammar-alist' to add more:
;;   (add-to-list 'template-literals-ts-tag-grammar-alist '("sql" . sql))

;;; Code:

(require 'treesit)
(require 'js)
(require 'css-mode)
(require 'html-ts-mode)

(defgroup template-literals-ts nil
  "Tree-sitter support for HTML/CSS in JS/TS template literals."
  :group 'languages
  :prefix "template-literals-ts-")

(defcustom template-literals-ts-tag-grammar-alist
  '(("html" . html) ("css" . css))
  "Alist mapping template literal tag names to tree-sitter grammars.
Each entry is (TAG-NAME . GRAMMAR) where TAG-NAME is the string
used as the tagged template function name (e.g. \"html\", \"css\",
\"sql\") and GRAMMAR is the tree-sitter grammar symbol to use for
parsing the template contents (e.g. `html', `css', `sql').

To add SQL support, for example:
  (add-to-list \\='template-literals-ts-tag-grammar-alist
               \\='(\"sql\" . sql))"
  :type '(alist :key-type string :value-type symbol)
  :group 'template-literals-ts)

(defcustom template-literals-ts-grammar-style-alist
  '((css . css) (html . html) (lit-html . html))
  "Alist mapping grammar symbols to their style for indent and font-lock.
The style determines indentation and which font-lock rules are applied:
- `css': Uses `css-indent-offset', counts \"block\" nodes for depth,
  fontifies selectors and properties
- `html': Uses `sgml-basic-offset', counts \"element\" nodes for depth,
  fontifies tags, attributes, and attribute values

Grammars not listed here get no font-lock rules and fall back to a
default indent of 2 with no depth tracking."
  :type '(alist :key-type symbol :value-type (choice (const css) (const html)))
  :group 'template-literals-ts)

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
  (let ((grammars (mapcar #'cdr template-literals-ts-tag-grammar-alist)))
    (or (seq-some
         (lambda (lang)
           (when-let* ((parser (seq-find
                                (lambda (p) (eq (treesit-parser-language p) lang))
                                (treesit-parser-list))))
             (when (seq-some (lambda (range)
                               (and (<= (car range) point)
                                    (< point (cdr range))))
                             (treesit-parser-included-ranges parser))
               lang)))
         grammars)
        template-literals-ts--host-language)))

(defun template-literals-ts--template-base-column ()
  "Get the base indentation column for the current template string.
Finds the template_string node containing point, then calculates
the indentation of the opening backtick plus one indent unit.
Returns nil if not inside a template string."
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
             ('typescript (or typescript-ts-mode-indent-offset 2))
             (_ (or js-indent-level 2))))))))

(defun template-literals-ts--in-template-p ()
  "Return t if point is inside template string content, nil if on opening line.
This distinguishes between being on the line with the opening backtick
versus being inside the actual template content, which affects indentation."
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
  "Calculate indentation for CSS/HTML inside template literals.
Returns the indentation offset in columns based on the tree-sitter
parse tree structure.  The indent style for each grammar is looked
up in `template-literals-ts-grammar-style-alist'.

For css style:
- Counts nested 'block' nodes to determine depth
- Adds extra indent for properties inside rule sets
- Decreases depth for closing braces

For html style:
- Counts nested 'element' nodes to determine depth
- Skips the root document element for tag nodes

The indentation unit is determined by css-indent-offset for css
style and sgml-basic-offset for html style, defaulting to 2 if
the grammar has no entry in the alist."
  (let* ((lang (treesit-language-at (point)))
         (style (alist-get lang template-literals-ts-grammar-style-alist))
         (indent-unit (pcase style
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
        (when (member type (pcase style
                             ('css '("block"))
                             ('html '("element"))))
          ;; For HTML: skip root element only for tag nodes, not content
          (unless (and (eq style 'html)
                       (equal type "element")
                       (equal (treesit-node-type grandparent) "document")
                       (member node-type '("<" ">" "</" "/>" "tag_name")))
            (setq depth (1+ depth))
            (setq found-block t))))
      (setq parent (treesit-node-parent parent)))
    (when (and (eq style 'css)
               in-rule-set
               (not found-block)
               (not (member node-type
                            '("class_selector" "id_selector" "tag_name"
                              "." "#" "selectors" "class_name"))))
      (setq depth 1))
    (when (and (eq style 'css)
               (equal node-type "}"))
      (setq depth (1- depth)))
    (* indent-unit depth)))

(defun template-literals-ts--indent-line ()
  "Indent line for template literals with embedded CSS/HTML.
If point is inside a CSS or HTML template literal, calculates
indentation as the sum of the template's base column and the
inner language's indentation. Otherwise, falls back to the
standard treesit-indent function."
  (treesit-update-ranges)
  (let ((lang (treesit-language-at (point))))
    (if (and (memq lang (mapcar #'cdr template-literals-ts-tag-grammar-alist))
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

  ;; Create a parser for each configured grammar
  (dolist (entry template-literals-ts-tag-grammar-alist)
    (treesit-parser-create (cdr entry)))

  ;; Generate range rules dynamically from the alist
  (setq-local treesit-range-settings
              (apply #'treesit-range-rules
                     (mapcan
                      (lambda (entry)
                        (let ((tag (car entry))
                              (grammar (cdr entry)))
                          (list :embed grammar
                                :host template-literals-ts--host-language
                                `((call_expression
                                   function: (identifier) @_tag
                                   arguments: (template_string
                                               (string_fragment) @capture)
                                   (:equal @_tag ,tag))))))
                      template-literals-ts-tag-grammar-alist)))

  (treesit-update-ranges)
  (setq-local treesit-language-at-point-function
              #'template-literals-ts--language-at-point)
  (setq-local indent-line-function #'template-literals-ts--indent-line)

  ;; Add font-lock rules for each configured grammar based on its
  ;; indent style in `template-literals-ts-grammar-style-alist'.
  (let ((new-rules '()))
    (dolist (entry template-literals-ts-tag-grammar-alist)
      (let* ((grammar (cdr entry))
             (style (alist-get grammar template-literals-ts-grammar-style-alist)))
        (pcase style
          ('css
           (setq new-rules
                 (append new-rules
                         (treesit-font-lock-rules
                          :language grammar
                          :feature 'selector
                          :override t
                          '((class_selector (class_name) @font-lock-type-face)
                            (id_selector (id_name) @font-lock-keyword-face)
                            (tag_name) @font-lock-function-name-face)
                          :language grammar
                          :feature 'property
                          :override t
                          '((property_name) @font-lock-property-name-face
                            (plain_value) @font-lock-constant-face
                            (color_value) @font-lock-constant-face
                            (integer_value) @font-lock-number-face
                            (float_value) @font-lock-number-face)))))
          ('html
           (setq new-rules
                 (append new-rules
                         (treesit-font-lock-rules
                          :language grammar
                          :feature 'tag
                          :override t
                          '((tag_name) @font-lock-function-name-face
                            (attribute_name) @font-lock-property-name-face
                            (quoted_attribute_value) @font-lock-string-face))))))))
    (when new-rules
      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings new-rules))))

  (font-lock-flush))

;;;###autoload
(define-minor-mode template-literals-ts-mode
  "Minor mode for syntax support in JS/TS tagged template literals.

Provides tree-sitter based syntax highlighting and indentation for
tagged template literals.  The tag names and grammars are configured
via `template-literals-ts-tag-grammar-alist' (defaults to html and css)."
  :lighter " TplLit"
  :group 'template-literals-ts
  (if template-literals-ts-mode
      (template-literals-ts--setup)
    (message
     "Disabling template-literals-ts-mode requires reopening the buffer")))

(provide 'template-literals-ts-mode)

;;; template-literals-ts-mode.el ends here

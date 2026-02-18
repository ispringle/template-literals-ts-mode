;;; template-literals-ts-mode-tests.el --- Tests for template-literals-ts-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for template-literals-ts-mode.
;;
;; Requires tree-sitter grammars: javascript, html, css.
;; Tests are skipped if grammars are unavailable.
;;
;; Run with:
;;   emacs -batch -l ert -l template-literals-ts-mode.el \
;;         -l template-literals-ts-mode-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'template-literals-ts-mode)

(defconst tltm-test--js-content
  "const greeting = html`<div class=\"hello\">world</div>`;
const style = css`.hello { color: red; }`;
const x = 42;
")

(defconst tltm-test--js-multiline
  "const tpl = html`
  <div class=\"outer\">
    <span>hello</span>
  </div>
`;

const styles = css`
  .outer {
    color: red;
  }
`;
")

(defmacro tltm-test--with-js-buffer (content &rest body)
  "Insert CONTENT into a temp buffer with js-ts-mode and the minor mode, then run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (js-ts-mode)
     (template-literals-ts-mode)
     ;; Force range computation
     (treesit-update-ranges)
     ,@body))

(defun tltm-test--grammars-available-p (&rest grammars)
  "Return non-nil if all GRAMMARS are available for tree-sitter."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (seq-every-p #'treesit-language-available-p grammars)))

;;; Parser creation

(ert-deftest tltm-test-creates-default-parsers ()
  "The default config should create css and html parsers."
  (skip-unless (tltm-test--grammars-available-p 'javascript 'html 'css))
  (tltm-test--with-js-buffer tltm-test--js-content
    (should (seq-find (lambda (p) (eq (treesit-parser-language p) 'css))
                      (treesit-parser-list)))
    (should (seq-find (lambda (p) (eq (treesit-parser-language p) 'html))
                      (treesit-parser-list)))))

(ert-deftest tltm-test-creates-parsers-from-custom-alist ()
  "Parsers should be created for each grammar in the custom alist."
  (skip-unless (tltm-test--grammars-available-p 'javascript 'css))
  (let ((template-literals-ts-tag-grammar-alist '(("css" . css))))
    (tltm-test--with-js-buffer tltm-test--js-content
      (should (seq-find (lambda (p) (eq (treesit-parser-language p) 'css))
                        (treesit-parser-list)))
      ;; html parser should NOT be created when not in the alist
      (should-not (seq-find (lambda (p) (eq (treesit-parser-language p) 'html))
                            (treesit-parser-list))))))

;;; Language at point

(ert-deftest tltm-test-language-at-point-js ()
  "Point in plain JS code should return javascript."
  (skip-unless (tltm-test--grammars-available-p 'javascript 'html 'css))
  (tltm-test--with-js-buffer tltm-test--js-content
    ;; "const x = 42;" is plain JS
    (goto-char (point-min))
    (search-forward "const x")
    (should (eq (template-literals-ts--language-at-point (point)) 'javascript))))

(ert-deftest tltm-test-language-at-point-html ()
  "Point inside html`...` should return html."
  (skip-unless (tltm-test--grammars-available-p 'javascript 'html 'css))
  (tltm-test--with-js-buffer tltm-test--js-content
    (goto-char (point-min))
    (search-forward "hello")
    (should (eq (template-literals-ts--language-at-point (point)) 'html))))

(ert-deftest tltm-test-language-at-point-css ()
  "Point inside css`...` should return css."
  (skip-unless (tltm-test--grammars-available-p 'javascript 'html 'css))
  (tltm-test--with-js-buffer tltm-test--js-content
    (goto-char (point-min))
    (search-forward "color")
    (should (eq (template-literals-ts--language-at-point (point)) 'css))))

;;; Range settings

(ert-deftest tltm-test-html-ranges-set ()
  "The html parser should have ranges covering html template content."
  (skip-unless (tltm-test--grammars-available-p 'javascript 'html 'css))
  (tltm-test--with-js-buffer tltm-test--js-content
    (let* ((parser (seq-find (lambda (p) (eq (treesit-parser-language p) 'html))
                             (treesit-parser-list)))
           (ranges (treesit-parser-included-ranges parser)))
      (should ranges)
      (should (> (length ranges) 0)))))

(ert-deftest tltm-test-css-ranges-set ()
  "The css parser should have ranges covering css template content."
  (skip-unless (tltm-test--grammars-available-p 'javascript 'html 'css))
  (tltm-test--with-js-buffer tltm-test--js-content
    (let* ((parser (seq-find (lambda (p) (eq (treesit-parser-language p) 'css))
                             (treesit-parser-list)))
           (ranges (treesit-parser-included-ranges parser)))
      (should ranges)
      (should (> (length ranges) 0)))))

;;; Indentation

(ert-deftest tltm-test-indent-html-multiline ()
  "HTML inside a template literal should be indented relative to the backtick."
  (skip-unless (tltm-test--grammars-available-p 'javascript 'html 'css))
  (tltm-test--with-js-buffer tltm-test--js-multiline
    ;; Go to the <span> line and indent it
    (goto-char (point-min))
    (search-forward "<span>")
    (beginning-of-line)
    (let ((before-indent (current-indentation)))
      (indent-according-to-mode)
      ;; <span> is nested inside <div>, so it should have more indent than <div>
      (goto-char (point-min))
      (search-forward "<div")
      (beginning-of-line)
      (let ((div-indent (current-indentation)))
        (goto-char (point-min))
        (search-forward "<span>")
        (beginning-of-line)
        (should (> (current-indentation) div-indent))))))

(ert-deftest tltm-test-indent-css-multiline ()
  "CSS inside a template literal should be indented relative to the backtick."
  (skip-unless (tltm-test--grammars-available-p 'javascript 'html 'css))
  (tltm-test--with-js-buffer tltm-test--js-multiline
    ;; Go to the "color: red;" line and indent it
    (goto-char (point-min))
    (search-forward "color: red")
    (beginning-of-line)
    (indent-according-to-mode)
    ;; color: red is inside .outer {}, so should be indented more than .outer
    (let ((prop-indent (current-indentation)))
      (goto-char (point-min))
      (search-forward ".outer")
      (beginning-of-line)
      (indent-according-to-mode)
      (should (> prop-indent (current-indentation))))))

;;; Customization

(ert-deftest tltm-test-alist-default-value ()
  "The default alist should contain html and css."
  (let ((default (eval (car (get 'template-literals-ts-tag-grammar-alist
                                  'standard-value)))))
    (should (equal (assoc "html" default) '("html" . html)))
    (should (equal (assoc "css" default) '("css" . css)))))

(ert-deftest tltm-test-alist-override-via-push ()
  "Pushing a new entry for an existing tag should shadow the old one."
  (let ((template-literals-ts-tag-grammar-alist
         (copy-alist template-literals-ts-tag-grammar-alist)))
    (push '("html" . lit-html) template-literals-ts-tag-grammar-alist)
    ;; assoc finds the first match
    (should (eq (cdr (assoc "html" template-literals-ts-tag-grammar-alist))
                'lit-html))))

(ert-deftest tltm-test-alist-add-new-grammar ()
  "Adding a new entry should be reflected in the grammars list."
  (let ((template-literals-ts-tag-grammar-alist
         (cons '("sql" . sql)
               (copy-alist template-literals-ts-tag-grammar-alist))))
    (should (eq (cdr (assoc "sql" template-literals-ts-tag-grammar-alist))
                'sql))
    (should (memq 'sql (mapcar #'cdr template-literals-ts-tag-grammar-alist)))))

(provide 'template-literals-ts-mode-tests)

;;; template-literals-ts-mode-tests.el ends here

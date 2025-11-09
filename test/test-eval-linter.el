;;; test-eval-linter.el --- ERT tests for eval linter  -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../src/gnu-lab-commands-eval.el" (file-name-directory load-file-name)))

(ert-deftest eval-linter-accepts-simple ()
  (should (gnu-lab-safe-elisp-p "(+ 1 2)")))

(ert-deftest eval-linter-denies-process ()
  (should-not (gnu-lab-safe-elisp-p "(call-process \"ls\")")))

(ert-deftest eval-linter-denies-funcall-to-forbidden ()
  (should-not (gnu-lab-safe-elisp-p "(funcall 'call-process \"ls\")")))

(ert-deftest eval-linter-allows-funcall-to-plus ()
  (should (gnu-lab-safe-elisp-p "(funcall '+ 1 2)")))

(ert-deftest eval-command-parsing-error-vs-deny ()
  (let ((ev '(:chat-id 1)))
    ;; Parse error: broken parentheses -> E_PARSE
    (let* ((fxs (gnu-lab-cmd-eval ev "(+ 1 2"))
           (fx (car fxs)))
      (should (equal (plist-get fx :type) :reply))
      (should (string-match-p "E_PARSE" (plist-get fx :text))))
    ;; Deny error: forbidden call-process -> E_DENY
    (let* ((fxs (gnu-lab-cmd-eval ev "(call-process \"ls\")"))
           (fx (car fxs)))
      (should (equal (plist-get fx :type) :reply))
      (should (string-match-p "E_DENY" (plist-get fx :text))))))

(ert-deftest eval-linter-denies-dynamic-callee-if ()
  ;; funcall with non-symbol callee must be denied.
  (should-not (gnu-lab-safe-elisp-p "(funcall (if t '+ 'call-process) 1 2)")))

(ert-deftest eval-linter-denies-symbol-function-indirection ()
  ;; symbol-function is not whitelisted; also callee is non-symbol at callsite.
  (should-not (gnu-lab-safe-elisp-p "(funcall (symbol-function 'call-process) \"ls\")")))

(ert-deftest eval-linter-denies-deep-ast ()
  ;; Build a nested form exceeding depth limit (e.g., 70 levels of (list ...)).
  (let* ((n 70)
         (s "0"))
    (dotimes (_ n)
      (setq s (concat "(list " s ")")))
    (should-not (gnu-lab-safe-elisp-p s))))

(ert-deftest eval-linter-denies-large-ast ()
  ;; Build a very large list of constants to exceed node limit.
  (let* ((n 2100)
         (s "(list"))
    (dotimes (_ n) (setq s (concat s " 1")))
    (setq s (concat s ")"))
    (should-not (gnu-lab-safe-elisp-p s))))

(ert-deftest eval-linter-denies-load ()
  (should-not (gnu-lab-safe-elisp-p "(load \"x\")")))

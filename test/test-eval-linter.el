;;; test-eval-linter.el --- ERT tests for eval linter  -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../src/gnu-lab-commands-eval.el" (file-name-directory load-file-name)))

(ert-deftest eval-linter-accepts-simple ()
  (should (gnu-lab-safe-elisp-p "(+ 1 2)")))

(ert-deftest eval-linter-denies-process ()
  (should-not (gnu-lab-safe-elisp-p "(call-process \"ls\")")))

(ert-deftest eval-linter-denies-load ()
  (should-not (gnu-lab-safe-elisp-p "(load \"x\")")))

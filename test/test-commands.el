;;; test-commands.el --- ERT tests for command parsing  -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../src/gnu-lab-commands.el" (file-name-directory load-file-name)))

(ert-deftest parse-command-name-and-args ()
  (let* ((text "/eval (+ 1 2)")
         (res (funcall (intern "gnu-lab--extract-cmd") text)))
    (should (equal (car res) "/eval"))
    (should (string-match-p "(\\+ 1 2)" (cadr res)))))

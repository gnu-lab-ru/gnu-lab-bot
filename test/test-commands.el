;;; test-commands.el --- ERT tests for command parsing  -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../src/gnu-lab-commands.el" (file-name-directory load-file-name)))
(load (expand-file-name "../src/gnu-lab-commands-basic.el" (file-name-directory load-file-name)))
(load (expand-file-name "../src/gnu-lab-commands-eval.el" (file-name-directory load-file-name)))

(ert-deftest parse-command-name-and-args ()
  (let* ((text "/eval (+ 1 2)")
         (res (funcall (intern "gnu-lab--extract-cmd") text)))
    (should (equal (car res) "/eval"))
    (should (string-match-p "(\\+ 1 2)" (cadr res)))))

(ert-deftest parse-command-with-bot-suffix ()
  (let* ((text "/eval@my_bot (+ 1 2 3)")
         (res (funcall (intern "gnu-lab--extract-cmd") text)))
    (should (equal (car res) "/eval"))
    (should (string-match-p "(\\+ 1 2 3)" (cadr res)))))

(ert-deftest help-lists-commands ()
  (gnu-lab-register-basic-commands)
  (gnu-lab-register-eval-command)
  (let* ((ev '(:chat-id 1))
         (fx (car (gnu-lab-cmd-help ev nil)))
         (txt (plist-get fx :text)))
    (should (string-match-p "/help" txt))
    (should (string-match-p "/ping" txt))
    (should (string-match-p "/eval" txt))))

;;; test-ratelimit.el --- ERT tests for rate limit  -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../src/gnu-lab-commands.el" (file-name-directory load-file-name)))

(ert-deftest ratelimit-per-user-deny-second ()
  ;; Register a temporary command with per-user limit 1 per 60 sec.
  (cl-letf (((symbol-function 'gnu-lab--commands) gnu-lab--commands))
    (defcommand "rl" :doc "rl test" :args nil :roles '(:user)
      :rate-limit '(:per-user 1 :window-sec 60)
      :handler (lambda (ev _args) (list (fx-reply :chat-id (plist-get ev :chat-id) :text "ok"))))
    (let* ((ev '(:kind :tg/message :chat-id 10 :from-id 42 :text "/rl")))
      ;; First call allowed
      (let ((fxs (gnu-lab-dispatch-command ev)))
        (should (equal (plist-get (car fxs) :type) :reply)))
      ;; Second call denied (E_RATE)
      (let ((fxs (gnu-lab-dispatch-command ev)))
        (should (equal (plist-get (car fxs) :type) :log))
        (should (equal (plist-get (cadr fxs) :type) :reply))
        (should (string-match-p "E_RATE" (plist-get (cadr fxs) :text)))))))

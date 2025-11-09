;;; test-ratelimit.el --- ERT tests for rate limit  -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../src/gnu-lab-commands.el" (file-name-directory load-file-name)))
(load (expand-file-name "../src/gnu-lab-commands-eval.el" (file-name-directory load-file-name)))

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

(ert-deftest ratelimit-eval-per-user-deny-second ()
  ;; Configure /eval rate-limit via env and verify E_RATE on second call.
  (setenv "EVAL_RATE_USER" "1")
  (setenv "RATE_WINDOW_SEC" "60")
  ;; Reset buckets if present (best-effort).
  (when (boundp 'gnu-lab--rl-user) (clrhash gnu-lab--rl-user))
  (when (boundp 'gnu-lab--rl-chat) (clrhash gnu-lab--rl-chat))
  ;; Re-register eval to apply new limits.
  (gnu-lab-register-eval-command)
  (let ((ev '(:kind :tg/message :chat-id 10 :from-id 99 :text "/eval (+ 1 2)")))
    ;; First call allowed -> should produce :run-sandbox effect.
    (let ((fxs (gnu-lab-dispatch-command ev)))
      (should (equal (plist-get (car fxs) :type) :run-sandbox)))
    ;; Second call denied -> expect :log then :reply with E_RATE.
    (let ((fxs (gnu-lab-dispatch-command ev)))
      (should (equal (plist-get (car fxs) :type) :log))
      (should (equal (plist-get (cadr fxs) :type) :reply))
      (should (string-match-p "E_RATE" (plist-get (cadr fxs) :text))))))
  ;; Cleanup env
  (setenv "EVAL_RATE_USER" nil)
  (setenv "RATE_WINDOW_SEC" nil))

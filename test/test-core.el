;;; test-core.el --- ERT tests for core  -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../src/gnu-lab-core.el" (file-name-directory load-file-name)))
(load (expand-file-name "../src/gnu-lab-commands-basic.el" (file-name-directory load-file-name)))
(load (expand-file-name "../src/gnu-lab-commands-eval.el" (file-name-directory load-file-name)))

(ert-deftest core-handle-unknown-command ()
  (let ((ev '(:kind :tg/message :chat-id 1 :text "/unknown")))
    (should (listp (gnu-lab/handle-event ev)))))

(ert-deftest core-eval-effect-generated ()
  (gnu-lab-register-eval-command)
  (let ((ev '(:kind :tg/message :chat-id 1 :text "/eval (+ 1 2)")))
    (let ((fxs (gnu-lab/handle-event ev)))
      (should (equal (plist-get (car fxs) :type) :run-sandbox)))))

(ert-deftest core-post-success-effects-store-offset ()
  (let* ((ev '(:kind :tg/message :update-id 41 :chat-id 1 :text "/ping"))
         (eff (gnu-lab/post-success-effects ev)))
    (should (listp eff))
    (should (equal (plist-get (car eff) :type) :store))
    (should (eq (plist-get (car eff) :key) 'telegram-offset))
    (should (equal (plist-get (car eff) :value) 42))))

(ert-deftest core-post-success-effects-no-update ()
  (let* ((ev '(:kind :tg/message :chat-id 1 :text "/ping"))
         (eff (gnu-lab/post-success-effects ev)))
    (should (null eff))))

;;; test-effects.el --- ERT tests for effects  -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../src/gnu-lab-effects.el" (file-name-directory load-file-name)))

(ert-deftest effects-valid-reply ()
  (should (gnu-lab-fx/valid-p (fx-reply :chat-id 1 :text "hi"))))

(ert-deftest effects-invalid-reply ()
  (should-not (gnu-lab-fx/valid-p (fx-reply :chat-id "x" :text 123))))

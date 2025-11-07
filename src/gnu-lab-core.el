;;; gnu-lab-core.el --- Core loop and interpreter  -*- lexical-binding: t; -*-

(require 'json)
(load "gnu-lab-effects.el")
(load "gnu-lab-config.el")
(load "gnu-lab-logger.el")
(load "gnu-lab-commands.el")
(load "gnu-lab-sandbox.el")
(load "gnu-lab-telegram.el")
(load "gnu-lab-storage.el")

(defun gnu-lab/handle-event (event)
  "Pure dispatcher: Event -> list of Effects."
  (gnu-lab-dispatch-command event))

(defun gnu-lab/perform-effects (effects)
  (mapcar
   (lambda (fx)
     (unless (gnu-lab-fx/valid-p fx)
       (gnu-lab-log-error `(:msg "invalid effect" :fx ,fx))
       (cl-return-from gnu-lab/perform-effects nil))
     (pcase (plist-get fx :type)
       (:reply (gnu-lab-telegram-send fx))
       (:run-sandbox
        (let* ((limits (plist-get fx :limits))
               (expr (plist-get fx :expr))
               (res (gnu-lab-sandbox-run-elisp expr limits))
               (chat (plist-get fx :chat-id)))
          (cond
           ((plist-get res :timeout-p)
            (gnu-lab-telegram-send (fx-reply :chat-id chat :text "Ошибка: E_TIMEOUT")))
           ((plist-get res :ok)
            (let ((out (or (plist-get res :stdout) "")))
              (gnu-lab-telegram-send (fx-reply :chat-id chat :text out))))
           (t
            (gnu-lab-telegram-send (fx-reply :chat-id chat :text "Ошибка: E_SANDBOX"))))))
       (:store (gnu-lab-storage-set (plist-get fx :key) (plist-get fx :value)))
       (:log (pcase (plist-get fx :level)
               (:info (gnu-lab-log-info (plist-get fx :data)))
               (:warn (gnu-lab-log-warn (plist-get fx :data)))
               (:error (gnu-lab-log-error (plist-get fx :data)))))
       (_ (gnu-lab-log-error `(:msg "unknown effect" :fx ,fx)))))
   effects))

(defun gnu-lab-run ()
  (gnu-lab-log-info '(:msg "starting" :component "core"))
  (gnu-lab-telegram-longpoll-loop
   (lambda (event)
     (condition-case e
         (let ((effects (gnu-lab/handle-event event)))
           (gnu-lab/perform-effects effects))
       (error (gnu-lab-log-error `(:msg "handler error" :error ,(format "%s" e))))))))

(provide 'gnu-lab-core)

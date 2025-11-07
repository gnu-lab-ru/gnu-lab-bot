;;; gnu-lab-effects.el --- Effects DSL  -*- lexical-binding: t; -*-

(defun gnu-lab-fx/make (type &rest kv)
  (cons :type (cons type kv)))

(defun gnu-lab-fx/type (fx) (plist-get fx :type))

(defun gnu-lab-fx/valid-p (fx)
  (let ((type (gnu-lab-fx/type fx)))
    (pcase type
      (:reply (and (integerp (plist-get fx :chat-id))
                   (stringp (plist-get fx :text))))
      (:run-sandbox (and (stringp (plist-get fx :expr))
                         (plist-get fx :limits)))
      (:store (and (symbolp (plist-get fx :key))))
      (:log (and (memq (plist-get fx :level) '(:info :warn :error))
                 (plist-get fx :data)))
      (_ nil))))

(defun fx-reply (&rest kv) (apply #'gnu-lab-fx/make :reply kv))
(defun fx-run-sandbox (&rest kv) (apply #'gnu-lab-fx/make :run-sandbox kv))
(defun fx-store (&rest kv) (apply #'gnu-lab-fx/make :store kv))
(defun fx-log (&rest kv) (apply #'gnu-lab-fx/make :log kv))

(provide 'gnu-lab-effects)

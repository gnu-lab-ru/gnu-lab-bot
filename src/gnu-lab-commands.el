;;; gnu-lab-commands.el --- Command DSL and dispatch  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(load "gnu-lab-effects.el")
(load "gnu-lab-ratelimit.el")

(defvar gnu-lab--commands (make-hash-table :test 'equal))

(cl-defun defcommand (name &key doc args roles rate-limit handler)
  (puthash (format "/%s" name)
           (list :name name :doc doc :args args :roles roles :rate-limit rate-limit :handler handler)
           gnu-lab--commands))

(defun gnu-lab--extract-cmd (text)
  (when (and (stringp text) (string-prefix-p "/" text))
    (let* ((parts (split-string text "[ \n]" t))
           (token (car parts))
           (at-pos (and token (string-match-p "@" token)))
           (name (if at-pos (substring token 0 at-pos) token))
           (args (string-trim (substring text (min (length text) (length token))))))
      (list name args))))

(defun gnu-lab-dispatch-command (event)
  (let* ((text (plist-get event :text))
         (pair (gnu-lab--extract-cmd text))
         (cmd (and pair (car pair)))
         (args (and pair (cadr pair)))
         (spec (and cmd (gethash cmd gnu-lab--commands))))
    (if (not spec)
        (list
         (fx-log :level :warn :data `(:msg "unknown command" :code "E_PARSE" :cmd ,cmd))
         (fx-reply :chat-id (plist-get event :chat-id)
                   :text "Неизвестная команда. Используйте /help"))
      (let ((rl (plist-get spec :rate-limit)))
        (when rl
          (unless (gnu-lab-ratelimit-allow-p event rl)
            (cl-return-from gnu-lab-dispatch-command
              (list
               (fx-log :level :warn :data `(:msg "rate limit" :code "E_RATE" :cmd ,cmd
                                                 :from-id ,(plist-get event :from-id)
                                                 :chat-id ,(plist-get event :chat-id)))
               (fx-reply :chat-id (plist-get event :chat-id) :text "Ошибка: E_RATE"))))))
      (let ((handler (plist-get spec :handler)))
        (funcall handler event args)))))

(provide 'gnu-lab-commands)

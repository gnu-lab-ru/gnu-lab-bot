;;; gnu-lab-telegram.el --- Telegram transport (MVP)  -*- lexical-binding: t; -*-

(require 'json)
(load "gnu-lab-config.el")
(load "gnu-lab-effects.el")
(load "gnu-lab-logger.el")
(load "gnu-lab-storage.el")

(defun gnu-lab--tg-api (method)
  (let ((token (gnu-lab-config-get 'telegram-token nil)))
    (unless (and token (not (string-empty-p token)))
      (error "TELEGRAM_BOT_TOKEN missing"))
    (format "https://api.telegram.org/bot%s/%s" token method)))

(defun gnu-lab-telegram--http-post-json (url payload)
  (with-temp-buffer
    (let* ((tmp (make-temp-file "tg" nil ".json" (json-encode payload)))
           (cmd (format "curl -sS -X POST -H 'Content-Type: application/json' --data '@%s' %s" (shell-quote-argument tmp) (shell-quote-argument url)))
           (status (call-process "sh" nil (current-buffer) nil "-lc" cmd)))
      (delete-file tmp)
      (when (not (eq status 0))
        (gnu-lab-log-error `(:msg "curl failed" :status ,status :url ,url)))
      (buffer-string))))

(defun gnu-lab-telegram-send (fx)
  (let* ((chat (plist-get fx :chat-id))
         (text (plist-get fx :text))
         (url (gnu-lab--tg-api "sendMessage"))
         (truncated (if (> (length text) 4000) (concat (substring text 0 4000) "…") text))
         (payload `((chat_id . ,chat) (text . ,truncated))))
    (gnu-lab-telegram--http-post-json url payload)))

(defun gnu-lab-telegram--normalize-update (upd)
  (let* ((msg (alist-get 'message upd))
         (chat (alist-get 'chat msg))
         (from (alist-get 'from msg)))
    (list :kind :tg/message
          :chat-id (alist-get 'id chat)
          :message-id (alist-get 'message_id msg)
          :from-id (alist-get 'id from)
          :from-username (alist-get 'username from)
          :text (alist-get 'text msg)
          :timestamp (alist-get 'date msg))))

(defun gnu-lab-telegram--get-updates (offset)
  (let* ((url (gnu-lab--tg-api "getUpdates"))
         (payload (delq nil `((timeout . 30) ,(when offset `(offset . ,offset)))))
         (resp (gnu-lab-telegram--http-post-json url payload)))
    (condition-case e
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read-from-string resp)))
          (alist-get 'result data))
      (error (progn (gnu-lab-log-error `(:msg "telegram parse error" :err ,(format "%s" e)))
                    nil)))))

(defun gnu-lab-telegram-longpoll-loop (callback)
  (let ((offset (or (gnu-lab-storage-last-offset) 0))
        (sleep 0.5))
    (while t
      (condition-case e
          (let ((updates (gnu-lab-telegram--get-updates offset)))
            (dolist (u updates)
              (let ((upd-id (alist-get 'update_id u)))
                (setq offset (max offset (1+ upd-id)))
                (gnu-lab-storage-save-offset offset)
                (let ((ev (gnu-lab-telegram--normalize-update u)))
                  (funcall callback ev)))))
        (error (gnu-lab-log-error `(:msg "telegram error" :err ,(format "%s" e)))
               (sleep-for sleep)
               (setq sleep (min 8.0 (* 2 sleep))))))))

(provide 'gnu-lab-telegram)

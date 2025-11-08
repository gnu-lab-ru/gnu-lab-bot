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
        (gnu-lab-log-error `(:msg "curl failed" :code "E_TELEGRAM" :component "telegram" :status ,status :url ,url)))
      (buffer-string))))

(defun gnu-lab-telegram-send (fx)
  (let* ((chat (plist-get fx :chat-id))
         (text (plist-get fx :text))
         (parse (plist-get fx :parse-mode))
         (url (gnu-lab--tg-api "sendMessage"))
         (truncated (if (> (length text) 4000) (concat (substring text 0 4000) "…") text))
         (payload (append `((chat_id . ,chat) (text . ,truncated))
                          (when parse `((parse_mode . ,(pcase parse
                                                         (:markdown "Markdown")
                                                         (:plain nil)
                                                         (_ nil)))))))
         (resp (gnu-lab-telegram--http-post-json url payload)))
    (condition-case e
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read-from-string resp)))
          (unless (alist-get 'ok data)
            (gnu-lab-log-error `(:msg "telegram sendMessage error" :code "E_TELEGRAM" :component "telegram" :response ,data))))
      (error (gnu-lab-log-error `(:msg "telegram send parse error" :code "E_TELEGRAM" :component "telegram" :err ,(format "%s" e)))))))

(defun gnu-lab-telegram--normalize-update (upd)
  (let* ((msg (alist-get 'message upd))
         (chat (alist-get 'chat msg))
         (from (alist-get 'from msg)))
    (list :kind :tg/message
          :update-id (alist-get 'update_id upd)
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
          (if (alist-get 'ok data)
              (alist-get 'result data)
            (progn
              (gnu-lab-log-error `(:msg "telegram getUpdates error" :code "E_TELEGRAM" :component "telegram" :response ,data))
              nil)))
      (error (progn (gnu-lab-log-error `(:msg "telegram parse error" :code "E_TELEGRAM" :component "telegram" :err ,(format "%s" e)))
                    nil)))))

(defun gnu-lab-telegram-longpoll-loop (callback)
  (let ((offset (or (gnu-lab-storage-last-offset) 0))
        (sleep 0.5))
    (while t
      (condition-case e
          (progn
            (let ((updates (gnu-lab-telegram--get-updates offset)))
              (dolist (u updates)
                (let* ((upd-id (alist-get 'update_id u))
                       (ev (gnu-lab-telegram--normalize-update u))
                       (ok (funcall callback ev)))
                  (when ok
                    (setq offset (max offset (1+ upd-id)))))))
            ;; Reset backoff on success.
            (setq sleep 0.5))
        (error
         (gnu-lab-log-error `(:msg "telegram error" :code "E_TELEGRAM" :component "telegram" :err ,(format "%s" e)))
         (sleep-for sleep)
         (setq sleep (min 8.0 (* 2 sleep))))))))

(provide 'gnu-lab-telegram)

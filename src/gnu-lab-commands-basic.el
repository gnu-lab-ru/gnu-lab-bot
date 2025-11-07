;;; gnu-lab-commands-basic.el --- Basic commands  -*- lexical-binding: t; -*-

(load "gnu-lab-effects.el")
(load "gnu-lab-commands.el")
(load "gnu-lab-config.el")

(defun gnu-lab--reply (event text)
  (list (fx-reply :chat-id (plist-get event :chat-id) :text text)))

(defun gnu-lab-cmd-start (event _args)
  (gnu-lab--reply event "Привет! Доступные команды: /help, /ping, /version, /eval EXPR"))

(defun gnu-lab-cmd-help (event _args)
  (gnu-lab--reply event "Команды:
/start — приветствие
/help — список команд
/ping — pong
/version — версии
/eval EXPR — безопасный eval"))

(defun gnu-lab-cmd-ping (event _args)
  (gnu-lab--reply event "pong"))

(defun gnu-lab-cmd-version (event _args)
  (gnu-lab--reply event (format "Мастерская Emacs бот — протокол %s" (gnu-lab-config-get 'protocol-version "1.0"))))

(defun gnu-lab-register-basic-commands ()
  (defcommand "start" :doc "Приветствие" :args nil :roles '(:user) :handler #'gnu-lab-cmd-start)
  (defcommand "help" :doc "Справка" :args nil :roles '(:user) :handler #'gnu-lab-cmd-help)
  (defcommand "ping" :doc "Проверка" :args nil :roles '(:user) :handler #'gnu-lab-cmd-ping)
  (defcommand "version" :doc "Версия" :args nil :roles '(:user) :handler #'gnu-lab-cmd-version))

(provide 'gnu-lab-commands-basic)

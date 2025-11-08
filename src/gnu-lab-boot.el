;;; gnu-lab-boot.el --- Entry point  -*- lexical-binding: t; -*-

;; This file is the entry script for running the bot in batch mode.
(require 'subr-x)

(load (expand-file-name "gnu-lab-config.el" (file-name-directory load-file-name)))
(load (expand-file-name "gnu-lab-logger.el" (file-name-directory load-file-name)))
(load (expand-file-name "gnu-lab-effects.el" (file-name-directory load-file-name)))
(load (expand-file-name "gnu-lab-storage.el" (file-name-directory load-file-name)))
(load (expand-file-name "gnu-lab-commands.el" (file-name-directory load-file-name)))
(load (expand-file-name "gnu-lab-commands-basic.el" (file-name-directory load-file-name)))
(load (expand-file-name "gnu-lab-commands-eval.el" (file-name-directory load-file-name)))
(load (expand-file-name "gnu-lab-sandbox.el" (file-name-directory load-file-name)))
(load (expand-file-name "gnu-lab-core.el" (file-name-directory load-file-name)))
(load (expand-file-name "gnu-lab-telegram.el" (file-name-directory load-file-name)))

(defun gnu-lab-main ()
  (gnu-lab-log-info '(:msg "boot" :component "boot"))
  (let ((token (gnu-lab-config-get 'telegram-token nil)))
    (if (and token (not (string-empty-p token)))
        (progn
          (gnu-lab-register-basic-commands)
          (gnu-lab-register-eval-command)
          (gnu-lab-run))
      (progn
        (gnu-lab-log-warn '(:msg "no TELEGRAM_BOT_TOKEN; dry boot" :component "boot"))
        (gnu-lab-register-basic-commands)
        (gnu-lab-register-eval-command)
        (gnu-lab-log-info '(:msg "dry-run complete" :component "boot"))))))

(gnu-lab-main)
(provide 'gnu-lab-boot)

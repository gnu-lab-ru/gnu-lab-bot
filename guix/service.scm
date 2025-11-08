;;; Shepherd service for the bot
(use-modules (gnu services)
             (gnu services shepherd)
             (gnu services base)
             (gnu packages)
             (gnu packages guile)
             (guix gexp)
             (guix packages)
             (guix profiles)
             (srfi srfi-1))

(define (gnu-lab-shepherd-service token-file)
  (shepherd-service
   (provision '(gnu-lab-bot))
   (documentation "Мастерская Emacs bot service")
   (start #~(make-forkexec-constructor
             (list
              ;; Run packaged launcher from emacs-gnu-lab; token is provided via env file.
              #$(file-append (specification->package "emacs-gnu-lab") "/bin/gnu-lab-run-bot"))
             #:environment-variables
             (list (string-append "TELEGRAM_BOT_TOKEN_FILE=" #$token-file))))
   (stop #~(make-kill-destructor))))

(define-public gnu-lab-bot-service-type
  (service-type
   (name 'gnu-lab-bot)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (lambda (config) (list (gnu-lab-shepherd-service (car config)))))))
   (compose identity)
   (extend identity)
   (default-value '("./secrets/telegram.token"))
   (description "Shepherd service for Мастерская Emacs bot")))

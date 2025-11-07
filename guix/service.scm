;;; Shepherd service for the bot
(use-modules (gnu services)
             (gnu services shepherd)
             (gnu services base)
             (gnu packages)
             (gnu packages guile)
             (guix gexp)
             (srfi srfi-1))

(define (gnu-lab-shepherd-service token-file)
  (shepherd-service
   (provision '(gnu-lab-bot))
   (documentation "Мастерская Emacs bot service")
   (start #~(make-forkexec-constructor
             (list #$(file-append (canonical-package (car (list (specification->package "coreutils")))) "/bin/env")
                   "TELEGRAM_BOT_TOKEN=$(cat " #$token-file ")"
                   "./scripts/run-bot")
             #:directory #$output))
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

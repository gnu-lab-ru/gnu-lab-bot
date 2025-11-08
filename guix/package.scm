;;; Guix package for the bot
(use-modules (guix packages)
             (guix git-download)
             (guix build-system emacs)
             (guix gexp)
             (guix licenses)
             (gnu packages)
             (gnu packages emacs)
             (gnu packages base))

(define-public emacs-gnu-lab
  (package
    (name "emacs-gnu-lab")
    (version "0.1.0")
    (source #f) ; local checkout build
    (build-system emacs-build-system)
    (arguments
     (list
      ;; Install helper scripts into $out/bin for service usage.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-scripts
            (lambda _
              (let* ((out #$output)
                     (bin (string-append out "/bin")))
                (mkdir-p bin)
                (when (file-exists-p "scripts/run-bot")
                  (copy-file "scripts/run-bot" (string-append bin "/gnu-lab-run-bot"))
                  (chmod (string-append bin "/gnu-lab-run-bot") #o755))
                (when (file-exists-p "scripts/eval-sandbox")
                  (copy-file "scripts/eval-sandbox" (string-append bin "/gnu-lab-eval-sandbox"))
                  (chmod (string-append bin "/gnu-lab-eval-sandbox") #o755))))))))
    (inputs (list coreutils util-linux))
    (home-page "https://example.org/gnu-lab")
    (synopsis "Мастерская Emacs — Telegram-бот")
    (description "Чисто-функциональный Telegram-бот с безопасным eval и Guix-развёртыванием.")
    (license gpl3+)))

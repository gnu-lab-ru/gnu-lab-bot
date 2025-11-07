;;; Guix package for the bot
(use-modules (guix packages)
             (guix git-download)
             (guix build-system emacs)
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
    (inputs (list coreutils util-linux))
    (home-page "https://example.org/gnu-lab")
    (synopsis "Мастерская Emacs — Telegram-бот")
    (description "Чисто-функциональный Telegram-бот с безопасным eval и Guix-развёртыванием.")
    (license gpl3+)))

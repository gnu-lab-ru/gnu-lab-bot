;;; gnu-lab-config.el --- Config handling  -*- lexical-binding: t; -*-

(defun gnu-lab-config-get (key default)
  (pcase key
    ('telegram-token (or (getenv "TELEGRAM_BOT_TOKEN") default))
    ('eval-timeout-sec (string-to-number (or (getenv "EVAL_TIMEOUT_SEC") "2")))
    ('eval-cpu-sec (string-to-number (or (getenv "EVAL_CPU_SEC") "2")))
    ('eval-mem-mb (string-to-number (or (getenv "EVAL_MEM_MB") "128")))
    ('eval-out-bytes (string-to-number (or (getenv "EVAL_OUT_BYTES") "8192")))
    ;; Rate-limit configs (MAY override defaults; nil means "use default in caller")
    ('eval-rate-user (let ((s (getenv "EVAL_RATE_USER"))) (and s (string-to-number s))))
    ('eval-rate-chat (let ((s (getenv "EVAL_RATE_CHAT"))) (and s (string-to-number s))))
    ('rate-window-sec (let ((s (getenv "RATE_WINDOW_SEC"))) (and s (string-to-number s))))
    ('log-level (intern (or (getenv "LOG_LEVEL") "info")))
    ('protocol-version (or (getenv "PROTOCOL_VERSION") "1.0"))
    (_ default)))

(provide 'gnu-lab-config)

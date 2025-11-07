;;; gnu-lab-config.el --- Config handling  -*- lexical-binding: t; -*-

(defun gnu-lab-config-get (key default)
  (pcase key
    ('telegram-token (or (getenv "TELEGRAM_BOT_TOKEN") default))
    ('eval-timeout-sec (string-to-number (or (getenv "EVAL_TIMEOUT_SEC") "2")))
    ('eval-cpu-sec (string-to-number (or (getenv "EVAL_CPU_SEC") "2")))
    ('eval-mem-mb (string-to-number (or (getenv "EVAL_MEM_MB") "128")))
    ('eval-out-bytes (string-to-number (or (getenv "EVAL_OUT_BYTES") "8192")))
    ('log-level (intern (or (getenv "LOG_LEVEL") "info")))
    (_ default)))

(provide 'gnu-lab-config)

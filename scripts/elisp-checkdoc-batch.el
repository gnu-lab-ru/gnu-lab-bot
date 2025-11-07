;;; elisp-checkdoc-batch.el --- Batch checkdoc runner  -*- lexical-binding: t; -*-

;; Minimal batch runner to check docstrings/style via checkdoc across files.
;; Exits with status 1 if any issues are detected.

(require 'checkdoc)

(setq sentence-end-double-space nil)

(let* ((files (or command-line-args-left '()))
       (had-issues nil))
  (dolist (f files)
    (princ (format "== checkdoc %s ==\n" f))
    (with-current-buffer (find-file-noselect f)
      (when (checkdoc-current-buffer t) ; non-nil if issues; prints messages
        (setq had-issues t))))
  (kill-emacs (if had-issues 1 0)))

;;; elisp-checkdoc-batch.el ends here

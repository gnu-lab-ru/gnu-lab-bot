;;; gnu-lab-ratelimit.el --- In-memory rate limits  -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar gnu-lab--rl-user (make-hash-table :test 'eql))
(defvar gnu-lab--rl-chat (make-hash-table :test 'eql))

(defun gnu-lab--rl-now () (float-time (current-time)))

(defun gnu-lab--rl-check-and-inc (table key limit window-sec)
  "Return non-nil if allowed and increment the counter; otherwise nil."
  (let* ((now (gnu-lab--rl-now))
         (cell (gethash key table)))
    (cond
     ((null limit) t) ; unlimited
     ((null cell)
      (puthash key (cons now 1) table)
      t)
     (t
      (let ((start (car cell))
            (count (cdr cell)))
        (if (> (- now start) window-sec)
            (progn
              (puthash key (cons now 1) table)
              t)
          (if (< count limit)
              (progn
                (puthash key (cons start (1+ count)) table)
                t)
            nil)))))))

(defun gnu-lab-ratelimit-allow-p (event rate-limit)
  "Check per-user and per-chat limits; update buckets. Return t if allowed."
  (let* ((per-user (plist-get rate-limit :per-user))
         (per-chat (plist-get rate-limit :per-chat))
         (window (or (plist-get rate-limit :window-sec) 60))
         (uid (plist-get event :from-id))
         (cid (plist-get event :chat-id)))
    (and
     (gnu-lab--rl-check-and-inc gnu-lab--rl-user (cons uid window) per-user window)
     (gnu-lab--rl-check-and-inc gnu-lab--rl-chat (cons cid window) per-chat window))))

(provide 'gnu-lab-ratelimit)

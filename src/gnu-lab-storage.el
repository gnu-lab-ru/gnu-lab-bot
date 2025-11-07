;;; gnu-lab-storage.el --- Simple file-based storage  -*- lexical-binding: t; -*-

(defun gnu-lab--state-dir ()
  (let ((d (expand-file-name "state" (locate-user-emacs-file "gnu-lab" default-directory))))
    (unless (file-directory-p d) (make-directory d t))
    d))

(defun gnu-lab-storage--file (name)
  (expand-file-name name (gnu-lab--state-dir)))

(defun gnu-lab-storage-get (key)
  (let* ((file (gnu-lab-storage--file (format "%s.sexp" key))))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (car (read-from-string (buffer-string)))))))

(defun gnu-lab-storage-set (key value)
  (let* ((file (gnu-lab-storage--file (format "%s.sexp" key)))
         (tmp (concat file ".tmp")))
    (with-temp-file tmp
      (prin1 value (current-buffer)))
    (rename-file tmp file t)))

(defun gnu-lab-storage-last-offset ()
  (gnu-lab-storage-get 'telegram-offset))

(defun gnu-lab-storage-save-offset (offset)
  (gnu-lab-storage-set 'telegram-offset offset))

(provide 'gnu-lab-storage)

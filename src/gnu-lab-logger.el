;;; gnu-lab-logger.el --- Structured logging  -*- lexical-binding: t; -*-

(defun gnu-lab--ts ()
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun gnu-lab--log (level data)
  (let ((payload (list :ts (gnu-lab--ts) :level level :data data)))
    (princ (concat (prin1-to-string payload) "\n"))))

(defun gnu-lab-log-info (data) (gnu-lab--log :info data))
(defun gnu-lab-log-warn (data) (gnu-lab--log :warn data))
(defun gnu-lab-log-error (data) (gnu-lab--log :error data))

(provide 'gnu-lab-logger)

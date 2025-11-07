;;; gnu-lab-commands.el --- Command DSL and dispatch  -*- lexical-binding: t; -*-

(defvar gnu-lab--commands (make-hash-table :test 'equal))

(cl-defun defcommand (name &key doc args roles rate-limit handler)
  (puthash (format "/%s" name)
           (list :name name :doc doc :args args :roles roles :rate-limit rate-limit :handler handler)
           gnu-lab--commands))

(defun gnu-lab--extract-cmd (text)
  (when (and (stringp text) (string-prefix-p "/" text))
    (let* ((parts (split-string text "[ \n]" t))
           (name (car parts))
           (args (string-trim (substring text (min (length text) (length name))))))
      (list name args))))

(defun gnu-lab-dispatch-command (event)
  (let* ((text (plist-get event :text))
         (pair (gnu-lab--extract-cmd text))
         (cmd (and pair (car pair)))
         (args (and pair (cadr pair)))
         (spec (and cmd (gethash cmd gnu-lab--commands))))
    (if (not spec)
        (list (fx-log :level :warn :data `(:msg "unknown command" :cmd ,cmd)))
      (let ((handler (plist-get spec :handler)))
        (funcall handler event args)))))

(provide 'gnu-lab-commands)

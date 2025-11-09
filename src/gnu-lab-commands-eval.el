;;; gnu-lab-commands-eval.el --- Secure eval command  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(load "gnu-lab-effects.el")
(load "gnu-lab-commands.el")
(load "gnu-lab-config.el")

(defun gnu-lab--symbol-allowed-p (sym)
  (memq sym '(quote if cond when unless let let* progn lambda and or
               + - * / = /= < <= > >= eq eql equal
               cons car cdr caar cadr cdar cddr list append length nth mapcar
               vector aref elt
               format concat substring string-match replace-regexp-in-string
               numberp stringp symbolp listp null not funcall apply)))

(defun gnu-lab--form-allowed-p (form depth nodes)
  (cond
   ((> depth 64) nil)
   ((> nodes 2000) nil)
   ((atom form)
    (cond ((symbolp form) (gnu-lab--symbol-allowed-p form))
          (t t)))
   ((consp form)
    (let* ((head (car form))
           (ok-head (and (symbolp head) (gnu-lab--symbol-allowed-p head)))
           (next-nodes (+ nodes 1)))
      (and ok-head
           ;; Special guard: funcall/apply allowed only if callee is a whitelisted symbol.
           (let ((callee (cadr form)))
             (if (memq head '(funcall apply))
                 (and (symbolp callee) (gnu-lab--symbol-allowed-p callee))
               t))
           (cl-every (lambda (x) (gnu-lab--form-allowed-p x (1+ depth) (1+ next-nodes)))
                     (cdr form)))))
   (t nil)))

(defun gnu-lab-safe-elisp-p (expr-string)
  (let ((read-eval nil))
    (condition-case _e
        (let* ((sexp (car (read-from-string expr-string))))
          (gnu-lab--form-allowed-p sexp 0 0))
      (error nil))))

(defun gnu-lab-cmd-eval (event args)
  (let ((expr (string-trim (or args ""))))
    (cond
     ((string-empty-p expr)
      (list (fx-reply :chat-id (plist-get event :chat-id) :text "Ошибка: E_PARSE")))
     ;; Parse phase to distinguish E_PARSE from E_DENY.
     ((let ((read-eval nil) parsed-ok)
        (condition-case _e
            (progn (car (read-from-string expr)) (setq parsed-ok t))
          (error (setq parsed-ok nil)))
        (not parsed-ok))
      (list (fx-reply :chat-id (plist-get event :chat-id) :text "Ошибка: E_PARSE")))
     ;; Validate whitelist.
     ((not (gnu-lab-safe-elisp-p expr))
      (list (fx-reply :chat-id (plist-get event :chat-id) :text "Ошибка: E_DENY")))
     (t
      (let* ((limits `(:timeout-sec ,(gnu-lab-config-get 'eval-timeout-sec nil)
                      :cpu-sec ,(gnu-lab-config-get 'eval-cpu-sec nil)
                      :mem-mb ,(gnu-lab-config-get 'eval-mem-mb nil)
                      :out-bytes ,(gnu-lab-config-get 'eval-out-bytes nil))))
        (list (fx-run-sandbox :chat-id (plist-get event :chat-id)
                              :expr expr :limits limits)))))))

(defun gnu-lab-register-eval-command ()
  (let* ((per-user (or (gnu-lab-config-get 'eval-rate-user nil) 5))
         (per-chat (or (gnu-lab-config-get 'eval-rate-chat nil) 20))
         (window   (or (gnu-lab-config-get 'rate-window-sec nil) 60)))
    (defcommand "eval"
      :doc "Безопасный eval в песочнице"
      :args '((:name expr :type string :required t))
      :roles '(:user)
      :rate-limit `(:per-user ,per-user :per-chat ,per-chat :window-sec ,window)
      :handler #'gnu-lab-cmd-eval)))

(provide 'gnu-lab-commands-eval)

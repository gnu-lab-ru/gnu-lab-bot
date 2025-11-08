;;; gnu-lab-sandbox.el --- Sandbox process runner  -*- lexical-binding: t; -*-

(require 'subr-x)
(load "gnu-lab-config.el")
(load "gnu-lab-logger.el")

(defun gnu-lab-sandbox--parse-output (buf)
  (with-current-buffer buf
    (goto-char (point-min))
    (let (stdout stderr status)
      (search-forward "---STDOUT---" nil t)
      (let ((start (point)))
        (search-forward "---STDERR---" nil t)
        (setq stdout (string-trim (buffer-substring-no-properties start (match-beginning 0)))))
      (let ((start (point)))
        (search-forward "---STATUS---" nil t)
        (setq stderr (string-trim (buffer-substring-no-properties start (match-beginning 0)))))
      (let ((start (point)))
        (setq status (string-to-number (buffer-substring-no-properties start (point-max)))))
      (list :ok (and (= status 0) (not (string-match-p "__TIMEOUT__" stdout)))
            :stdout stdout :stderr stderr
            :status status :timeout-p (or (= status 124) (string-match-p "__TIMEOUT__" stdout))))))

(defun gnu-lab-sandbox-run-elisp (expr limits)
  (let* ((timeout (plist-get limits :timeout-sec))
         (cpu (plist-get limits :cpu-sec))
         (mem (plist-get limits :mem-mb))
         (out (plist-get limits :out-bytes))
         (default-directory (or (getenv "TMPDIR") default-directory))
         (buf (generate-new-buffer "*gnu-lab-sandbox*")))
    (let ((process-environment (append (list (format "EVAL_TIMEOUT_SEC=%s" timeout)
                                             (format "EVAL_CPU_SEC=%s" cpu)
                                             (format "EVAL_MEM_MB=%s" mem)
                                             (format "EVAL_OUT_BYTES=%s" out))
                                       process-environment)))
      (unwind-protect
          (let* ((runner (or (executable-find "gnu-lab-eval-sandbox") "scripts/eval-sandbox"))
                 (status (call-process "sh" nil buf nil "-lc"
                                       (format "%s %s" runner (shell-quote-argument expr)))))
            (with-current-buffer buf
              (goto-char (point-min)))
            (let ((res (gnu-lab-sandbox--parse-output buf)))
              (cond
               ((plist-get res :timeout-p)
                (gnu-lab-log-warn `(:msg "sandbox timeout" :code "E_TIMEOUT" :component "sandbox"
                                          :limits ,limits :status ,(plist-get res :status))))
               ((not (plist-get res :ok))
                (gnu-lab-log-error `(:msg "sandbox nonzero status" :code "E_SANDBOX" :component "sandbox"
                                           :status ,(plist-get res :status)
                                           :stderr-bytes ,(length (or (plist-get res :stderr) ""))))))
              res))
        (kill-buffer buf)))))

(provide 'gnu-lab-sandbox)

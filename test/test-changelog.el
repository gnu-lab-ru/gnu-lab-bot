;;; test-changelog.el --- ERT tests for changelog  -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../src/gnu-lab-changelog.el" (file-name-directory load-file-name)))

(ert-deftest changelog-range-defaults ()
  (cl-destructuring-bind (from to)
      (cl-letf (((symbol-function 'gnu-lab-changelog--detect-last-tag) (lambda () nil)))
        (gnu-lab-changelog--range nil nil))
    (should (string= from ""))
    (should (string= to "HEAD"))))

(ert-deftest changelog-insert-at-top ()
  (let* ((tmp (make-temp-file "chlog" nil ".md" "Old\n"))
         (entry "## [0.1.1] — 2025-01-01\n### Added\n- Test"))
    (gnu-lab-changelog--insert-at-top tmp entry)
    (with-temp-buffer
      (insert-file-contents tmp)
      (goto-char (point-min))
      (should (looking-at-p "## \\[0.1.1\\]"))
      (search-forward "Old")
      (should t))))

(ert-deftest changelog-generate-with-stubs ()
  (cl-letf (((symbol-function 'gnu-lab-changelog--call-git)
             (lambda (&rest _a) "stub"))
            ((symbol-function 'gnu-lab-changelog--detect-last-tag)
             (lambda () "v0.1.0"))
            ((symbol-function 'gnu-lab-changelog--detect-current-tag)
             (lambda () "v0.1.1"))
            ((symbol-function 'gnu-lab-changelog--llm)
             (lambda (_p) "## [0.1.1] — 2025-01-01\n### Added\n- Stub")))
    (let ((tmp (make-temp-file "chlog" nil ".md")))
      (gnu-lab-changelog-generate-entry nil nil nil "2025-01-01" tmp 'ru)
      (with-temp-buffer
        (insert-file-contents tmp)
        (goto-char (point-min))
        (should (looking-at-p "## \\[0.1.1\\]"))))))

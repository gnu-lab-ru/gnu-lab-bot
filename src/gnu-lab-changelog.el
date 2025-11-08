;;; gnu-lab-changelog.el --- Changelog generation via gptel  -*- lexical-binding: t; -*-

;; Dev tool: generate CHANGELOG.md entries from git metadata using gptel.

(require 'subr-x)
(require 'cl-lib)

(declare-function gptel-request "gptel")

(defgroup gnu-lab-changelog nil
  "Changelog generation (dev tool) for gnu-lab-bot."
  :group 'tools)

(defcustom gnu-lab-changelog-file "CHANGELOG.md"
  "Target changelog file."
  :type 'string
  :group 'gnu-lab-changelog)

(defcustom gnu-lab-changelog-language 'ru
  "Language for generated entries."
  :type '(choice (const ru) (const en))
  :group 'gnu-lab-changelog)

(defcustom gnu-lab-changelog-max-context-bytes 200000
  "Max bytes of git context to feed the LLM."
  :type 'integer
  :group 'gnu-lab-changelog)

(defcustom gnu-lab-changelog-ignore-paths
  '("^state/" "^build/" "^dist/" "\\.elc$" "^\\.github/" "^\\.gitlab/" "^\\.dir-locals\\.el$"
    "^guix/channels\\.scm$")
  "List of regexps for files to ignore in context sent to LLM."
  :type '(repeat string)
  :group 'gnu-lab-changelog)

(defcustom gnu-lab-changelog-llm-timeout-sec 120
  "Timeout in seconds for waiting LLM response in batch mode."
  :type 'integer
  :group 'gnu-lab-changelog)

(defvar gnu-lab-changelog--llm-fn nil
  "Function to call the LLM. If non-nil, used as (fn PROMPT) -> string.
If nil, uses gptel synchronously at runtime.")

(defun gnu-lab-changelog--call-git (&rest args)
  "Run git with ARGS and return trimmed output string, or nil on non-zero exit."
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil (current-buffer) nil args)))
      (when (and (integerp status) (= status 0))
        (string-trim (buffer-string))))))

(defun gnu-lab-changelog--detect-last-tag ()
  "Return last tag name, or nil."
  (gnu-lab-changelog--call-git "describe" "--tags" "--abbrev=0"))

(defun gnu-lab-changelog--detect-current-tag ()
  "Return tag name if HEAD is exactly on a tag, else nil."
  (gnu-lab-changelog--call-git "describe" "--tags" "--exact-match"))

(defun gnu-lab-changelog--range (from to)
  "Compute range boundaries as (FROM TO).
If FROM is nil, use last tag or empty string (start of history).
If TO is nil, use HEAD."
  (list (or from (gnu-lab-changelog--detect-last-tag) "")
        (or to "HEAD")))

(defun gnu-lab-changelog--filter-paths (names)
  "Filter file name list NAMES by `gnu-lab-changelog-ignore-paths'."
  (cl-remove-if
   (lambda (n)
     (cl-some (lambda (re) (string-match-p re n))
              gnu-lab-changelog-ignore-paths))
   names))

(defun gnu-lab-changelog--collect-context (from to)
  "Collect textual git context between FROM and TO.
Includes: git log (subject+body), filtered file names and shortstat."
  (let* ((range (if (string-empty-p from) to (format "%s..%s" from to)))
         (log (gnu-lab-changelog--call-git "log" "--pretty=format:%h %an %ad%n%s%n%b%n---"
                                           "--date=short" range))
         (names-raw (gnu-lab-changelog--call-git "diff" "--name-only" range))
         (names-list (and names-raw (split-string names-raw "\n" t)))
         (names (when names-list
                  (string-join (gnu-lab-changelog--filter-paths names-list) "\n")))
         (stat (gnu-lab-changelog--call-git "diff" "--shortstat" range))
         (text (string-join
                (delq nil
                      (list (format "Range: %s" (if (string-empty-p from) range (format "%s..%s" from to)))
                            "== Commit log ==" log
                            "== Files changed ==" names
                            "== Shortstat ==" stat))
                "\n")))
    (if (> (string-bytes text) gnu-lab-changelog-max-context-bytes)
        (substring text 0 gnu-lab-changelog-max-context-bytes)
      text)))

(defun gnu-lab-changelog--format-date (lang &optional time)
  "Format current date for LANG. Russian: \"D месяц YYYY\"."
  (let* ((t0 (or time (current-time)))
         (dt (decode-time t0))
         (year (nth 5 dt))
         (month (nth 4 dt))
         (day (nth 3 dt)))
    (if (eq lang 'ru)
        (let* ((months '("января" "февраля" "марта" "апреля" "мая" "июня"
                         "июля" "августа" "сентября" "октября" "ноября" "декабря"))
               (m (nth (1- month) months)))
          (format "%d %s %04d" day m year))
      (format-time-string "%Y-%m-%d" t0))))

(defun gnu-lab-changelog--prompt (version date context lang)
  "Build LLM prompt for VERSION, DATE using CONTEXT. LANG is 'ru or 'en."
  (if (eq lang 'ru)
      (format
       (concat
        "Ты — помощник релиз-инженера. По данным git сгенерируй запись CHANGELOG "
        "для версии %s от %s. Формат строго Markdown (Keep a Changelog):\n"
        "## [%s] — %s\n"
        "### Added\n- ...\n"
        "### Changed\n- ...\n"
        "### Fixed\n- ...\n"
        "### Removed\n- ...\n"
        "### Docs\n- ...\n"
        "### Security\n- ...\n\n"
        "Правила:\n"
        "- Пиши кратко, объединяй однотипные изменения; используй Conventional Commits.\n"
        "- Русский язык, прошедшее время.\n"
        "- Если раздел пуст — опусти его.\n"
        "- Выведи только секцию версии, без преамбулы и постскриптума.\n\n"
        "Контекст git (без диффов):\n%s")
       version date version date context)
    (format
     (concat
      "You are a release engineer. Generate a Keep a Changelog entry "
      "for version %s (%s). Strict Markdown only:\n"
      "## [%s] — %s\n"
      "### Added\n- ...\n"
      "### Changed\n- ...\n"
      "### Fixed\n- ...\n"
      "### Removed\n- ...\n"
      "### Docs\n- ...\n"
      "### Security\n- ...\n\n"
      "Rules: concise, use Conventional Commits mapping, merge similar changes, omit empty sections, output only the version section.\n\n"
      "Git context (no diffs):\n%s")
     version date version date context)))

(defun gnu-lab-changelog--auto-version (to-tag)
  "Infer version from TO-TAG. If vX.Y.Z return X.Y.Z; else \"Unreleased\"."
  (if (and to-tag (string-match "^v\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" to-tag))
      (match-string 1 to-tag)
    "Unreleased"))

(defun gnu-lab-changelog--llm (prompt)
  "Call LLM with PROMPT, return string or signal error on timeout."
  (if (functionp gnu-lab-changelog--llm-fn)
      (funcall gnu-lab-changelog--llm-fn prompt)
    (condition-case _
        (progn
          (require 'gptel)
          (let ((result nil)
                (done nil)
                (deadline (+ (float-time) gnu-lab-changelog-llm-timeout-sec)))
            (gptel-request prompt
              :callback (lambda (resp _info)
                          (setq result (string-trim (or resp "")) done t)))
            (while (and (not done) (< (float-time) deadline))
              (accept-process-output nil 0.1))
            (unless done
              (error "LLM timeout"))
            result))
      (error (error "gptel not configured or call failed")))))

(defun gnu-lab-changelog--insert-at-top (file entry)
  "Insert ENTRY at the top of FILE, creating file if missing."
  (let ((exists (file-exists-p file)))
    (with-temp-buffer
      (when exists (insert-file-contents file))
      (goto-char (point-min))
      (insert entry "\n\n")
      (write-region (point-min) (point-max) file))))

;;;###autoload
(defun gnu-lab-changelog-generate-entry (&optional from-tag to-tag version date outfile lang)
  "Generate a changelog entry and insert into OUTFILE.

FROM-TAG and TO-TAG define git range; if nil, last tag to HEAD.
VERSION defaults to version inferred from TO-TAG (vX.Y.Z -> X.Y.Z) or \"Unreleased\".
DATE defaults: RU -> \"D месяц YYYY\"; EN -> ISO \"YYYY-MM-DD\".
OUTFILE defaults to `gnu-lab-changelog-file'.
LANG defaults to `gnu-lab-changelog-language'."
  (interactive)
  (cl-destructuring-bind (from to) (gnu-lab-changelog--range from-tag to-tag)
    (let* ((lang (or lang gnu-lab-changelog-language))
           (ver (or version
                    (gnu-lab-changelog--auto-version
                     (or to-tag (gnu-lab-changelog--detect-current-tag)))))
           (date (or date (gnu-lab-changelog--format-date lang)))
           (ctx (gnu-lab-changelog--collect-context from to))
           (prompt (gnu-lab-changelog--prompt ver date ctx lang))
           (entry (gnu-lab-changelog--llm prompt))
           (file (or outfile gnu-lab-changelog-file)))
      (unless (and entry (string-match-p "^## \\[" entry))
        (error "LLM did not return a valid changelog section"))
      (gnu-lab-changelog--insert-at-top file entry)
      (message "Changelog updated: %s" file))))

;;;###autoload
(defun gnu-lab-changelog-batch ()
  "Batch entrypoint.

Reads FROM_TAG, TO_TAG, VERSION, OUTFILE, LANG (ru|en) from environment."
  (let* ((from (getenv "FROM_TAG"))
         (to (getenv "TO_TAG"))
         (version (getenv "VERSION"))
         (outfile (or (getenv "OUTFILE") gnu-lab-changelog-file))
         (lang (pcase (downcase (or (getenv "LANG") (symbol-name gnu-lab-changelog-language)))
                 ((or "ru" "ru_ru" "ru-ru") 'ru)
                 (_ 'en))))
    (gnu-lab-changelog-generate-entry from to version nil outfile lang)))

(provide 'gnu-lab-changelog)
;;; gnu-lab-changelog.el ends here

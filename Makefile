# Minimal task runner for build/lint/test under Guix
EMACS ?= emacs

.PHONY: help env build lint test ci fmt run test-integ changelog

help:
	@echo "Targets:"
	@echo "  env        - show toolchain versions"
	@echo "  build      - byte-compile all Emacs Lisp files"
	@echo "  lint       - run unified linter (Elisp + Scheme/Guix)"
	@echo "  test       - run ERT tests in batch"
	@echo "  ci         - strict lint + tests (warnings as errors)"
	@echo "  fmt        - check Guix style (dry-run)"
	@echo "  run        - run the bot (needs TELEGRAM_BOT_TOKEN)"
	@echo "  test-integ - quick sandbox smoke test"
	@echo "  changelog  - generate CHANGELOG.md via gptel"

env:
	@echo "== guix describe ==" || true
	@guix describe || true
	@echo "== $(EMACS) --version =="
	@$(EMACS) --version | head -n1

build:
	@echo "== byte-compile =="
	$(EMACS) -Q --batch -L src \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $$(find src test -maxdepth 1 -type f -name '*.el')

lint:
	@scripts/lint

test:
	@echo "== ERT tests =="
	$(EMACS) -Q --batch -L src -L test \
	  -l ert \
	  --eval "(mapc #'load (directory-files \"test\" t \"^test-.*\\\\.el$$\"))" \
	  -f ert-run-tests-batch-and-exit

ci:
	@echo "== CI strict mode =="
	LINT_STRICT=1 scripts/lint
	$(MAKE) test

fmt:
	@echo "== guix style (dry) =="
	@guix style -n guix || true

run:
	@./scripts/run-bot

test-integ:
	@echo "== sandbox smoke =="
	@./scripts/eval-sandbox "(+ 1 2)" | sed -n '1,120p'

changelog:
	@./scripts/changelog

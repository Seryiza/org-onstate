EMACS ?= emacs

.PHONY: test compile check

test:
	$(EMACS) -Q --batch -L . -l org-onstate.el -l org-onstate-actions.el -l test/org-onstate-test.el -f ert-run-tests-batch-and-exit

# Compile copies so neither success nor failure leaves .elc files in the repo.
compile:
	@set -eu; \
	tmp=$$(mktemp -d "$${TMPDIR:-/tmp}/org-onstate-compile.XXXXXX"); \
	trap 'rm -rf "$$tmp"' EXIT HUP INT TERM; \
	cp org-onstate.el org-onstate-actions.el test/org-onstate-test.el "$$tmp/"; \
	$(EMACS) -Q --batch -L "$$tmp" \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile "$$tmp/org-onstate.el" "$$tmp/org-onstate-actions.el" "$$tmp/org-onstate-test.el"

check: test compile

CASK ?= cask
EMACS ?= emacs

.cask:
	@$(CASK)

.PHONY: test
test: .cask
	@$(CASK) exec buttercup -L .

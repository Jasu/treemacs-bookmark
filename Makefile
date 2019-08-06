CASK ?= cask
EMACS ?= emacs

.PHONY: test
test: .cask
	@$(CASK) exec buttercup -L .

.cask:
	@$(CASK)

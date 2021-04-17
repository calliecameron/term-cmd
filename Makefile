all: test

.PHONY: test
test:
	rm -rf dist
	rm -rf emacs.d/elpa/term-cmd-1.2
	cask package
	cask install
	cask exec ert-runner
	test/test-interactive.sh

clean:
	rm -rf dist emacs.d .cask *.elc *~ test/*~ bin/*~

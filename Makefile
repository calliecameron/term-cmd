all: test

.PHONY: test
test:
	cask install
	cask exec ert-runner
	test/test-interactive.sh

clean:
	rm -rf dist emacs.d *.elc *~ test/*~ bin/*~

all: test

.PHONY: test
test:
	cask install
	cask exec ert-runner

clean:
	rm -rf dist emacs.d *.elc *~ test/*~ bin/*~

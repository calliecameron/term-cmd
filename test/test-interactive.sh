#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "${THIS_DIR}/.." &&
cask package &&
cd ~ &&
emacs -Q --eval \
"(progn
   (setq debug-on-error t)
   (setq user-emacs-directory \"${THIS_DIR}/../emacs.d/\")
   (require 'package)
   (setq package-archives '((\"melpa-stable\" . \"https://stable.melpa.org/packages/\")
                            (\"melpa\" . \"https://melpa.org/packages/\")
                            (\"gnu\" . \"https://elpa.gnu.org/packages/\")))
   (package-initialize)
   (package-refresh-contents)
   (package-install-file \"${THIS_DIR}/../dist/term-cmd-1.1.tar\")
   (add-to-list
     'term-cmd-commands-alist
     '(\"foobar\" . (lambda (cmd arg)
                      (if
                        (and (string= cmd \"foobar\")
                             (string= arg \"foo\"))
                          (message \"Test succeeded.\")
                         (message \"Test failed.\")))))
   (ansi-term \"${THIS_DIR}/ansi-term-test.sh\")
   (sleep-for 5)
   (term-send-string (get-buffer-process (current-buffer)) \"emacs-term-cmd foobar foo\")
   (term-send-input))"

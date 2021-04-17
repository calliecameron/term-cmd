#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

EMACS_TO_RUN="${CASK_EMACS}"
if [ -z "${CASK_EMACS}" ]; then
    EMACS_TO_RUN='emacs'
fi

cd "${THIS_DIR}/.." &&
cask package &&
cd ~ &&
"${EMACS_TO_RUN}" -Q --eval \
"(progn
   (setq debug-on-error t)
   (setq user-emacs-directory \"${THIS_DIR}/../emacs.d/\")
   (require 'package)
   (setq package-archives '((\"melpa-stable\" . \"https://stable.melpa.org/packages/\")
                            (\"melpa\" . \"https://melpa.org/packages/\")
                            (\"gnu\" . \"https://elpa.gnu.org/packages/\")))
   (package-initialize)
   (package-refresh-contents)
   (package-install-file \"${THIS_DIR}/../dist/term-cmd-1.2.tar\")
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
   (term-send-string (get-buffer-process (current-buffer)) \"echo \\\"If the test succeeds, you should see 'Test succeeded' printed in the minibuffer below after a few seconds. Then you can quit emacs. 'Test failed' or nothing printed means the test failed.\\\"\")
   (term-send-input)
   (sleep-for 5)
   (term-send-string (get-buffer-process (current-buffer)) \"emacs-term-cmd foobar foo\")
   (term-send-input))"

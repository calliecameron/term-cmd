;;; term-cmd.el --- Send commands to Emacs from programs running under term.el

;; Copyright (C) 2014 Callum J. Cameron

;; Author: Callum J. Cameron <cjcameron7@gmail.com>
;; Version: 1.0
;; Url: https://github.com/CallumCameron/term-cmd
;; Keywords: processes

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Send commands to Emacs from programs running in the term.el
;; terminal emulator.
;;
;; In vanilla Emacs, programs running in the terminal emulator can
;; send commands back to Emacs by printing a 'magic escape sequence'
;; which the terminal emulator parses -- this is how directory
;; tracking works.  But the list of commands is hard-coded, and you
;; can't add new ones.
;;
;; This package lets you add new commands.  It uses a different magic
;; escape sequence to avoid interfering with the built-in commands,
;; but the principle is the same.  When a program prints a command, it
;; won't show up on the screen, but will be interpreted by Emacs
;; instead.
;;
;; Note: this is a library, and doesn't make any user-visible changes.
;; For an example of something that actually uses it, see the
;; 'term-alert' package (https://github.com/CallumCameron/term-alert).
;;
;;
;; Usage:
;;
;; To register a command:
;;
;;     (add-to-list 'term-cmd-commands-alist
;;                  '("command" . some-callback-function))
;;
;; where "command" is the name of the command, and
;; some-callback-function is the function you want to be called when
;; the command is run.  The function should take two arguments -- the
;; first is the command name itself, and the second is the command's
;; argument.
;;
;; To send a command, just print the magic escape sequence in some
;; program in the terminal:
;;
;;     # Using GNU printf
;;     /usr/bin/printf '\eTeRmCmD %s %s\n' "${CMD}" "${ARG}"
;;
;;     # With a shell built-in, the syntax might be different; here for ZSH:
;;     print "\033TeRmCmD ${CMD} ${ARG}"
;;
;; The '\e' and '\033' are the ESC character.  You can leave off the
;; argument if you don't need it.
;;
;; Alternatively, use the 'emacs-term-cmd' script:
;;
;;     emacs-term-cmd command arg
;;
;; Because the commands are based on terminal output, they work just
;; as well through nested shells, or multiple SSH sessions. See the
;; emacs-term-cmd script for how to pass commands through from tmux
;; (not 100% reliable, and unfortunately doesn't support screen).
;;
;;
;; Installation:
;;
;; Install the 'term-cmd' package from MELPA.
;;
;; Or, for a manual install:
;;
;; 1. Make sure this file is on your load path
;; 2. (require 'term-cmd)
;;
;; There is also a shell script, 'emacs-term-cmd', in this package's
;; git repository, which makes sending commands easier.

;;; Code:

(require 'term)

;;;###autoload
(defvar term-cmd-commands-alist '()
  "Commands to run based on process output.
Elements should be of the form (<string> . <func>) where string is the
command to match on, and func takes two args, the command and the
command's argument.  To run a command from the terminal, output a line
of the form '\\eTeRmCmD <command> <arg>\\n', where arg is an arbitrary
string; the function with key <command> will be called with command
and arg.  Arg can also be omitted if it is not required.")


;; These variables allow incomplete commands at the end of input to be
;; stored and handled when more input arrives.
(defvar term-cmd--partial-cmd nil)
(make-local-variable 'term-cmd--partial-cmd)

;; These variables do the same thing, but for Emacs' built-in
;; directory-tracking messages; see
;; term-cmd--ansi-partial-beginning-check and
;; term-cmd--ansi-partial-end-check, below.
(defvar term-cmd--partial-ansi-terminal-message nil)
(make-local-variable 'term-cmd--partial-ansi-terminal-message)

;;;###autoload
(defun term-cmd--do-command (message)
  "Scan MESSAGE for any commands, execute them, and return the remaining message."

  ;; Handle stored partial command
  (when term-cmd--partial-cmd
    (setq message (concat term-cmd--partial-cmd message))
    (setq term-cmd--partial-cmd nil))

  ;; Process the commands
  (while (string-match "\eTeRmCmD +\\(.+\\)\n" message)
    (let* ((matched (match-string 1 message))
           command
           arg)

      ;; Remove the command from the message that will eventually be printed
      (setq message (replace-match "" t t message))

      (when (string-match "\r$" matched)
        (setq matched (replace-match "" t t matched)))

      (if (string-match " " matched)
          (progn
            (setq command (substring matched 0 (match-beginning 0)))
            (setq arg (substring matched (match-end 0))))
        (setq command matched)
        (setq arg ""))

      (let ((func (assoc command term-cmd-commands-alist)))
        (if func
            (funcall (cdr func) command arg)
          (message "Unknown term-cmd command '%s'" command)))))

  ;; If there is a partial message at the end of the string, store it
  ;; for future use.
  (when (string-match "\eTeRmCmD.+$" message)
    (setq term-cmd--partial-cmd (match-string 0 message))
    (setq message (replace-match "" t t message)))

  message)


;; These functions handle partial messages at the end of the input,
;; but for Emacs' built-in directory-tracking commands rather than our
;; ones (the built-in commands use a different magic escape sequence,
;; only allow single-char commands, and are hardcoded in term.el -- so
;; not accessible to the user).  This is essentially a fix for a
;; term.el bug (see
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17231), but that patch
;; never got in, so I might as well do it here, right?

;;;###autoload
(defun term-cmd--ansi-partial-beginning-check (message)
  "Handle stored partial commands for built-in commands in MESSAGE."
  (when term-cmd--partial-ansi-terminal-message
    (setq message (concat term-cmd--partial-ansi-terminal-message message))
    (setq term-cmd--partial-ansi-terminal-message nil))
  message)

;;;###autoload
(defun term-cmd--ansi-partial-end-check (message)
  "Handle partial built-in commands at the end of MESSAGE."
  (when (string-match "\eAnSiT.+$" message)
    (setq term-cmd--partial-ansi-terminal-message (match-string 0 message))
    (setq message (replace-match "" t t message)))
  message)


;; The main advice that makes everything work.

;;;###autoload
(defadvice term-handle-ansi-terminal-messages (around term-cmd--advice activate)
  "Process any term-cmd commands before passing the remaining input on to term.el."
  (ad-set-arg 0 (term-cmd--do-command (ad-get-arg 0)))
  (ad-set-arg 0 (term-cmd--ansi-partial-beginning-check (ad-get-arg 0)))
  ad-do-it
  (setq ad-return-value (term-cmd--ansi-partial-end-check ad-return-value)))


(provide 'term-cmd)

;;; term-cmd.el ends here

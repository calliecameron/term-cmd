term-cmd
========

[![MELPA](https://melpa.org/packages/term-cmd-badge.svg)](https://melpa.org/#/term-cmd)
[![MELPA Stable](https://stable.melpa.org/packages/term-cmd-badge.svg)](https://stable.melpa.org/#/term-cmd)

Send commands to Emacs from programs running in term.el.

In vanilla Emacs, programs running in term.el can send commands back
to Emacs by printing a 'magic escape sequence' which the terminal
emulator parses -- this is how directory tracking works. But the list
of commands is hard-coded, and you can't add new ones.

This package lets you add new commands. It uses a different magic
escape sequence to avoid interfering with the built-in commands, but
the principle is the same. When a program prints a command, it won't
show up on the screen, but will instead be interpreted by Emacs.

This is a library, and doesn't make any user-visible changes. For an
example of something that uses it, see
[term-alert](https://github.com/CallumCameron/term-alert).


Usage
-----

To register a command:

    (add-to-list
     'term-cmd-commands-alist
     '("command" . some-callback-function))

where `"command"` is the name of the command, and
`some-callback-function` is the function you want to be called when
the command is run. The function should take two arguments -- the
first is the command name itself, and the second is the command's
argument.

To send a command, use the `emacs-term-cmd` script:

    emacs-term-cmd command arg

If called outside Emacs, this does nothing (i.e. it won't mess things
up).

Because the commands are based on terminal output, they work just as
well through nested shells, multiple SSH sessions, or tmux (not 100%
reliable -- see comments in `emacs-term-cmd`).


Installation
------------

Install the `term-cmd` package from MELPA.

The `emacs-term-cmd` command will always be on the `PATH` of any shell
launched from Emacs. However, for full functionality you should add
~/.emacs.d/term-cmd (or wherever your `user-emacs-directory` is) to
the `PATH` in your environment or shell's startup files, too
(e.g. ~/.profile, ~/.bashrc, ~/.zshrc, etc.), on any machine you often
SSH into; this will allow shells inside tmux or on other machines to
send commands back to Emacs on your local machine.


License
-------

Copyright (C) 2014--2016 Callum J. Cameron

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

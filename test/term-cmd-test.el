;;; term-cmd-test.el --- Term-cmd: tests.            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Callum Cameron

;; Author: Callum Cameron <callum@CallumPC>
;; Keywords: terminals

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

;;; Code:

(require 'f)

(defconst term-cmd-test--root (f-parent (f-parent load-file-name)))
(defconst term-cmd-test--bin (f-join term-cmd-test--root "bin"))

(setq user-emacs-directory (f-join term-cmd-test--root "emacs.d"))

(add-to-list 'load-path term-cmd-test--root)

(require 'term-cmd)

(ert-deftest term-cmd-executable ()
  (should (f-directory? term-cmd--bin-dir))
  (should (f-file? term-cmd--executable-abs))
  (should (-contains? exec-path term-cmd--bin-dir))
  (should (-contains? (s-split path-separator (getenv "PATH")) term-cmd--bin-dir))
  (should (string= (executable-find term-cmd--executable-name) term-cmd--executable-abs))
  (should (eq (call-process
               "cmp"
               nil
               nil
               nil
               "-s"
               (f-join term-cmd-test--bin term-cmd--executable-name)
               term-cmd--executable-abs)
              0)))

(ert-deftest term-cmd-advice ()
  (should (advice-member-p 'term-cmd--advice 'term-handle-ansi-terminal-messages)))

;;; term-cmd-test.el ends here

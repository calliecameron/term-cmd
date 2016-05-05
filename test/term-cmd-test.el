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

(add-to-list 'load-path term-cmd-test--root)

(require 'term-cmd)


(ert-deftest term-cmd-path ()
  (should (-contains? exec-path term-cmd-test--bin))
  (should (-contains? (s-split path-separator (getenv "PATH")) term-cmd-test--bin))
  (should (string= (executable-find "emacs-term-cmd") (f-join term-cmd-test--bin "emacs-term-cmd"))))

;;; term-cmd-test.el ends here

;;; mb-modes.el --- My modes configurations -*- lexical-binding: t; -*-

;; Copyright ⓒ 2017 Mattias Bengtsson
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with This program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Author: Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Version	    : 20170308
;; Keywords	    : local
;; Package-Requires : ((emacs "25.1"))
;; URL		    : https://github.com/moonlite/.emacs.d
;; Doc URL	    : TBA
;; Compatibility    : GNU Emacs: 25.x

;;; Commentary:

;; TODO: Move the setup code to a package and move the configuration to
;; customize.

;;; Note:

;;; Code:

(require 'mb-f)

(defconst mb-modes--filepattern-to-mode-map
  '(("\\.inl\\'"           . c++-mode)
    ("\\.ui$"              . nxml-mode)
    ("\\.j2$"              . jinja2-mode)
    ("\\.js$"              . js2-mode)
    ("\\.jshintrc$"        . js2-mode)
    ("\\.jscsrc$"          . json-mode)
    ("\\.ks$"              . sh-mode)
    ("\\.seed$"            . sh-mode)
    ("\\.geojson$"         . json-mode)
    ("\\.vala$"            . vala-mode)
    ("\\.mapcss$"          . css-mode)
    ("\\.mcss$"            . css-mode)
    ("\\.m$"               . octave-mode)
    ("\\.dec$"             . mtg-deck-mode)
    ("\/Cask$"             . emacs-lisp-mode)
    ("\/buildstream.conf$" . emacs-lisp-mode)
    ("\/evo"               . message-mode)
    ("\\.h$"               . mb-cmd-guess-cc-mode)))

(defconst mb-modes--shortened-major-modes
  '((markdown-mode   . "M↓")
    (js2-mode        . "JS")
    (nxml-mode       . "XML")
    (c-mode          . "C")
    (c++-mode        . "C++")
    (cmake-mode      . "CMake")
    (emacs-lisp-mode . "Elisp")
    (go-mode         . "Go")
    (haskell-mode    . "λ")
    (snippet-mode    . "Yas Snippet")))

(defconst mb-modes--shortened-minor-modes
  '((abbrev-mode                 . " A")
    (aggressive-indent-mode      . " ⇒")
    (anaconda-mode               . " 🐍")
    (auto-dim-other-buffers-mode . "")
    (auto-revert-mode            . " ⎌")
    (auto-sudoedit-mode          . "")
    (company-mode                . " C")
    (control-mode                . "")
    (eldoc-mode                  . " 🕮")
    (fancy-narrow-mode           . "")
    (flyspell-mode               . " ✎")
    (git-gutter-mode             . "")
    (haskell-indentation-mode    . "")
    (magit-auto-revert-mode      . "")
    (magit-filenotify-mode       . " Notify")
    (magit-gitflow-mode          . " Flow")
    (racer-mode                  . "")
    (sqlup-mode                  . " ⇑")
    (which-key-mode              . "")
    (ws-butler-mode              . " W")
    (yas-minor-mode              . " Y")))

(defun mb-modes-activate ()
  "Activate mode configurations."
  (mb-f-auto-modes          mb-modes--filepattern-to-mode-map)
  (mb-f-shorten-major-modes mb-modes--shortened-major-modes)
  (mb-f-shorten-minor-modes mb-modes--shortened-minor-modes))

(provide 'mb-modes)
;;; mb-modes.el ends here

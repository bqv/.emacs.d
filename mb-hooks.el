;;; mb-hooks.el --- My mode hooks -*- lexical-binding: t; -*-

;; Copyright ⓒ 2016 Mattias Bengtsson

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20160417
;; Keywords         : init
;; Package-Requires : ((emacs "25.1"))
;; URL              : https://github.com/moonlite/.emacs.d
;; Compatibility    : GNU Emacs: 25.x

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

;;; Commentary:

;;; Note:

;;; Code:

;; After Save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; AG
(defvar ag-mode-map)
(defun mb-hooks--ag-mode ()
  "My `ag' mode hook."
  (my/define-keys ag-mode-map
                  '(( "W" . wgrep-change-to-wgrep-mode))))

(add-hook 'ag-mode-hook #'mb-hooks--ag-mode)

;; Backward Forward
(defun mb-hooks--backward-forward-mode ()
  "My `backward-forward' mode hook."
  (defvar backward-forward-mode-map)
  (my/define-keys backward-forward-mode-map
                  '(("M-<left>"  . backward-forward-previous-location)
                    ("M-<right>" . backward-forward-next-location)
                    ("C-<left>"  . nil)
                    ("C-<right>" . nil))))

(add-hook 'backward-forward-mode-hook #'mb-hooks--backward-forward-mode)

;; Browse Kill Ring
(defvar browse-kill-ring-mode-map)
(defun mb-hooks--browse-kill-ring-mode ()
  "My `browse-kill-ring' mode hook."
  (my/define-keys browse-kill-ring-mode-map
                  '(( "<down>"    . browse-kill-ring-forward)
                    ( "<tab>"     . browse-kill-ring-forward)
                    ( "<up>"      . browse-kill-ring-previous)
                    ( "<backtab>" . browse-kill-ring-previous)
                    ( "C-g"       . browse-kill-ring-quit))))

(add-hook 'browse-kill-ring-mode-hook #'mb-hooks--browse-kill-ring-mode)

;; C common
(defvar c-mode-base-map)
(defun mb-hooks--c-common ()
  "My `c-mode' mode hook."
  (unless (keymap-parent c-mode-base-map)
    (set-keymap-parent c-mode-base-map prog-mode-map)))

(add-hook 'c-mode-common-hook #'mb-hooks--c-common)

;; C / C++
(defvar rtags-completions-enabled)
(defvar company-backends)
(defvar projectile-command-map)
(defun mb-hooks--c-mode ()
  "A mode hook for C and C++."
  (require 'rtags)
  (require 'company-rtags)
  (rtags-start-process-unless-running)
  (setq-local rtags-completions-enabled t)
  (rtags-enable-standard-keybindings c-mode-base-map)
  (setq-local company-backends '(company-rtags))

  ;; Work around bug where c-mode-base-map doesn't inherit from
  ;; prog-mode-map
  (my/define-keys c-mode-base-map
                  '(( "C-<return>" . rtags-find-symbol-at-point)
                    ( "C-z f r"    . rtags-rename-symbol)
                    ( "."          . my/dot-and-complete)
                    ( ":"          . my/double-colon-and-complete)
                    ( ">"          . my/arrow-and-complete)))
  (my/define-keys projectile-command-map
                  '(( "j"         . rtags-find-symbol))))

(add-hook 'c-mode-hook   #'mb-hooks--c-mode)
(add-hook 'c++-mode-hook #'mb-hooks--c-mode)

;; CMake
(defun mb-hooks--cmake-mode ()
  "My `cmake' mode hook."
  (setq-local company-backends '((company-cmake
                                  company-keywords
                                  company-files
                                  company-dabbrev-code))))

(add-hook 'cmake-mode-hook #'mb-hooks--prog-mode)
(add-hook 'cmake-mode-hook #'mb-hooks--cmake-mode)

;; Control
(defvar control-mode)
(defvar control-mode-keymap)
(defun mb-hooks--control-mode ()
  "My `control' mode hook."
  (my/control-mode-set-cursor)
  (my/define-keys control-mode-keymap
                  '(( "i"           . my/control-mode-off)
                    ( "<escape>"    . ESC-prefix)
                    ( "x x"         . smex)
                    ( "x s"         . save-buffer)
                    ( "x S"         . save-some-buffers))))

(add-hook 'control-mode-keymap-generation-functions
          'control-mode-ctrlx-hacks)
(add-hook 'control-mode-hook #'mb-hooks--control-mode)

;; Company
(defvar company-active-map)
(defun mb-hooks--company-mode ()
  "My `company' mode hook."
  (company-quickhelp-mode)

  ;; Make it possible to undo unwanted completion.
  (define-key company-active-map (kbd "<SPC>")
    '(lambda()
       (interactive)
       (insert " ")
       (undo-boundary)
       (company-complete-selection)))

  (my/define-keys company-active-map
                  '(( "\C-n"    . company-select-next)
                    ( "\C-p"    . company-select-previous)
                    ( "<next>"  . my/company-scroll-down)
                    ( "<prior>" . my/company-scroll-up)
                    ( "\C-v"    . company-show-location)
                    ( "\C-g"    . company-abort))))

(add-hook 'company-mode-hook                 #'mb-hooks--company-mode)
(add-hook 'company-completion-started-hook   #'my/fci-turn-off)
(add-hook 'company-completion-finished-hook  #'my/fci-turn-on)
(add-hook 'company-completion-cancelled-hook #'my/fci-turn-on)


;; Compilation Mode

(defvar compilation-filter-start)
(defun mb-hooks--compilation-filter ()
  "My `compilation-filter' mode hook."
  (require 'ansi-color)
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook #'mb-hooks--compilation-filter)

;; Cython
(defun mb-hooks--cython-mode ()
  "My `cython' mode hook."
  (require 'flycheck-cython))

(add-hook 'cython-mode-hook #'mb-hooks--cython-mode)

;; Dired
(defvar dired-mode-map)
(require 'dired-imenu)
(defun mb-hooks--dired-mode ()
  "My `dired' mode hook."
  (hl-line-mode)
  (dired-hide-details-mode)
  (my/define-keys dired-mode-map
                  '(( "W" . wdired-change-to-wdired-mode)
                    ( "F" . find-name-dired)
                    ( "c" . find-file)))
  (my/remap-keys dired-mode-map
                 '(("s" . "C-s")
                   ("r" . "C-r"))))

(add-hook 'dired-mode-hook #'mb-hooks--dired-mode)

;; ELisp
(defun mb-hooks--emacs-lisp-mode ()
  "My `emacs-lisp' mode hook."
  (setq page-delimiter
        (rx bol ";;;" (not (any "#")) (* not-newline) "\n"
            (* (* blank) (opt ";" (* not-newline)) "\n"))))

(add-hook 'emacs-lisp-mode-hook #'lisp-extra-font-lock-mode)
(add-hook 'emacs-lisp-mode-hook #'mb-hooks--emacs-lisp-mode)

;; Flycheck
(defun mb-hooks--flycheck-mode ()
  "My `flycheck' mode hook."
  (flycheck-pos-tip-mode)
  (flycheck-status-emoji-mode)
  (flycheck-cask-setup)
  (flycheck-package-setup))

(add-hook 'flycheck-mode-hook #'mb-hooks--flycheck-mode)

;; Flyspell
(defvar flyspell-mode-map)
(defun mb-hooks--flyspell-mode ()
  "My `flyspell' mode hook."
  (require 'flyspell-correct-popup)
  (flyspell-buffer)
  (my/define-keys flyspell-mode-map
                  '(("C-," . my/flyspell-goto-previous-error)
                    ("C-." . flyspell-goto-next-error)
                    ("C-;" . flyspell-correct-previous-word-generic)
                    ("C-:" . flyspell-correct-next-word-generic))))

(add-hook 'flyspell-mode-hook #'mb-hooks--flyspell-mode)

;; Find-file
(add-hook 'find-file-not-found-functions #'my/create-non-existent-directory)

;; Go
(defvar go-mode-map)
(defun mb-hooks--go-mode ()
  "My `go' mode hook."
  (go-eldoc-setup)

  (setq-local tab-width 4)
  (setq-local company-backends '(company-go
                                 company-keywords
                                 company-files))

  (my/define-keys go-mode-map
                  '(( "C-z i a"    . go-import-add)
                    ( "C-z i r"    . go-remove-unused-imports)
                    ( "C-z i g"    . go-goto-imports)
                    ( "C-z d"      . godoc-at-point)
                    ( "C-<return>" . godef-jump)
                    ( "."          . my/dot-and-complete))))

(add-hook 'go-mode-hook #'mb-hooks--go-mode)

;; Haskell
(defun mb-hooks--haskell-mode ()
  "My `haskell' mode hook."
  (setq-local electric-indent-mode nil)
  (turn-on-haskell-indentation))

(add-hook 'haskell-mode-hook #'mb-hooks--haskell-mode)

;; Help
(defvar help-mode-map)
(defun mb-hooks--help-mode ()
  "My `help' mode hook."
  (my/define-keys help-mode-map
                  '(( "M-<left>"  . help-go-back)
                    ( "M-<right>" . help-go-forward)))
  (my/remap-keys help-mode-map
                 '(("s" . "C-s")
                   ("r" . "C-r"))))

(add-hook 'help-mode-hook #'mb-hooks--help-mode)

;; Ido
(defvar ido-common-completion-map)
(defun mb-hooks--ido-setup ()
  "My `ido' mode hook."
  (my/define-keys ido-common-completion-map
                  '(( "<tab"    . ido-complete)
                    ( "<next>"  . my/ido-scroll-down)
                    ( "<prior>" . my/ido-scroll-up))))

(add-hook 'ido-setup-hook #'mb-hooks--ido-setup)

;; IBuffer
(defvar ibuffer-sorting-mode)
(defun mb-hooks--ibuffer ()
  "My `ibuffer' mode hook."
  (ibuffer-projectile-set-filter-groups)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(add-hook 'ibuffer-hook 'mb-hooks--ibuffer)

;; Iedit
(defvar iedit-mode-keymap)
(defun mb-hooks--iedit-mode ()
  "My `iedit' mode hook."
  (my/define-keys iedit-mode-keymap
                  '(("C-g"      . iedit-quit)
                    ("<return>" . iedit-quit))))

(add-hook 'iedit-mode-hook #'mb-hooks--iedit-mode)
(add-hook 'iedit-aborting-hook #'deactivate-mark)

;; IELM
(defvar ielm-map)
(defun mb-hooks--ielm-mode ()
  "My `ielm' mode hook."
  (company-mode)
  (my/define-keys ielm-map
                  '(( "<tab>" . my/snippet-or-complete))))

(add-hook 'ielm-mode-hook #'mb-hooks--ielm)

;; Info
(defun mb-hooks--Info-mode ()
  "My `Info' mode hook."
  (my/define-keys Info-mode-map
                  '(( "M-<left>"  . Info-history-back)
                    ( "M-<right>" . Info-history-forward)
                    ( "M-<up>"    . Info-up))))

(add-hook 'Info-mode-hook #'mb-hooks--Info-mode)
(add-hook 'Info-selection-hook #'niceify-info)

;; Java
(defun mb-hooks--java-mode ()
  "My `java' mode hook."
  (require 'ensime-company)
  (ensime)
  (setq-local company-backends '((ensime-company))))

(add-hook 'java-mode-hook #'mb-hooks--java-mode)

;; JS2
(defvar js2-mode-map)
(defvar flimenu-imenu-separator)
(autoload 'js2r-rename-var "js2-refactor" "" t nil)

(defun mb-hooks--js2-mode ()
  "My `js2' mode hook."
  (require 'js2-refactor)
  (js2-imenu-extras-mode)
  (setq flimenu-imenu-separator ".")
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)

  (my/define-keys js2-mode-map
                  '(( "C-z f r"    . js2r-rename-var)))

  (setq-local company-backends '((company-dabbrev-code
                                  company-files
                                  company-keywords))))

(add-hook 'js2-mode-hook #'mb-hooks--js2-mode)

(defvar tern-mode-keymap)
(defun mb-hooks--tern-mode ()
  "My `tern' mode hook."
  (my/define-keys tern-mode-keymap
                  '(( "C-<return>" . tern-find-definition)
                    ( "C-z f r"    . tern-rename-variable))))

(add-hook 'tern-mode-hook #'mb-hooks--tern-mode)

;; Todotxt
(autoload 'todotxt "todotxt" "" t nil)
(autoload 'todotxt-mode "todotxt" "" t nil)
(defvar todotxt-mode-map)
(defun mb-hooks--todotxt-mode ()
  "My `todotxt' mode hook."
  (my/define-keys todotxt-mode-map
                  '(("j" . nil)
                    ("k" . todotxt-nuke-item)
                    ("_" . todotxt-undo)
                    ("u" . nil))))

(add-hook 'todotxt-mode-hook #'mb-hooks--todotxt-mode)

;; JSON
(defun mb-hooks--json-mode ()
  "My `json' mode hook."
  (highlight-numbers-mode -1))

(add-hook 'json-mode-hook #'mb-hooks--json-mode)

;; Lua
(defun mb-hooks--lua-mode ()
  "My `lua' mode hook."
  (setq-local company-backends '((company-dabbrev-code
                                  company-files
                                  company-keywords))))

(add-hook 'lua-mode-hook #'mb-hooks--lua-mode)

;; Magit
(defun mb-hooks--git-commit-mode ()
  "My `git-commit' mode hook."
  (my/control-mode-off)
  (setq-local fill-column 72)
  (git-commit-turn-on-flyspell)
  (git-commit-turn-on-auto-fill)
  (fci-mode 1))

(autoload 'turn-on-magit-gitflow "magit-gitflow" "" t nil)
(defun mb-hooks--magit-mode ()
  "My `magit' mode hook."
  (require 'magit-gitflow)
  (turn-on-magit-gitflow)
  (add-hook 'magit-post-refresh-hook
            #'git-gutter:update-all-windows))

(defvar magit-blame-mode-map)
(defun mb-hooks--magit-blame-mode ()
  "My `magit-blame' mode hook."
  (my/define-keys magit-blame-mode-map
                  '(( "C-z t b"     .  magit-blame-quit))))

(add-hook 'magit-status-mode-hook #'magit-filenotify-mode)
(add-hook 'magit-blame-mode-hook  #'mb-hooks--magit-blame-mode)
(add-hook 'magit-mode-hook        #'mb-hooks--magit-mode)
(add-hook 'git-commit-mode-hook   #'mb-hooks--git-commit-mode)

;; Markdown
(defvar markdown-mode-map)
(defun mb-hooks--markdown-mode ()
  "My `markdown' mode hook."
  (flyspell-mode)
  (setq-local fill-column 80)
  (fci-mode)
  (auto-fill-mode)
  (setq-local indent-tabs-mode nil)
  (my/define-keys markdown-mode-map
                  '(( "C-<return>" . markdown-jump)
                    ( "C-c C-c p"  . my/open-with)
                    ( "M-<up>"     . nil)
                    ( "M-<down>"   . nil))))

(add-hook 'markdown-mode-hook #'mb-hooks--markdown-mode)

;; MTG deck mode
(defvar mtg-deck-mode-map)
(defun mb-hooks--mtg-deck-mode ()
  "My `mtg-deck' mode hook."
  (company-mode)
  (setq-local company-backends '(company-capf))
  (my/define-keys mtg-deck-mode-map
                  '(( "<tab>" . my/snippet-or-complete))))

(add-hook 'mtg-deck-mode-hook #'mb-hooks--mtg-deck-mode)

;; Multiple Cursors
(defun mb-hooks--multiple-cursors-mode-enabled ()
  "My `multiple-cursors' mode hook."
  (control-mode-reload-bindings))

(add-hook 'multiple-cursors-mode-enabled-hook
          #'mb-hooks--multiple-cursors-mode-enabled)

;; nXML
(defvar nxml-mode-map)
(defun mb-hooks--nxml-mode ()
  "My `nxml' mode hook."
  (setq-local company-backends '(company-nxml
                                 company-keywords
                                 company-files))
  (my/define-keys nxml-mode-map
                  '(( "<tab>" . my/snippet-or-complete))))

(add-hook 'nxml-mode-hook #'mb-hooks--nxml-mode)
(add-hook 'nxml-mode-hook #'mb-hooks--prog-mode)

;; Package Menu
(defun mb-hooks--package-menu-mode ()
  "My `package-menu' mode hook."
  (hl-line-mode)
  (my/remap-keys package-menu-mode-map
                 '(("s" . "C-s")
                   ("R" . "r")
                   ("r" . "C-r"))))

(add-hook 'package-menu-mode-hook #'mb-hooks--package-menu-mode)

;; Prog
(defvar flyspell-prog-text-faces)
(defun mb-hooks--prog-mode ()
  "My `prog-mode' hook."

  (setq-local fill-column 80)
  (unless (derived-mode-p 'makefile-mode)
    (setq-local indent-tabs-mode nil))

  (ws-butler-mode)
  (company-mode)
  (flycheck-mode)
  (setq flyspell-prog-text-faces
        '(font-lock-comment-face font-lock-doc-face))
  (flyspell-prog-mode)
  (fci-mode)
  (highlight-numbers-mode)
  (emr-initialize)
  (backward-forward-mode)

  (my/define-keys prog-mode-map
                  '(( "<tab>"       . my/snippet-or-complete)
                    ( "C-z f e"     . my/iedit-in-defun)
                    ( "C-z f f"     . emr-show-refactor-menu)
                    ( "C-z d"       . nil)
                    ( "C-z d d"     . my/realgud-debug)
                    ( "C-z d a"     . realgud-short-key-mode)))
  (my/remap-keys  prog-mode-map
                  '(( "RET"         . "M-j"))))

(add-hook 'prog-mode-hook #'mb-hooks--prog-mode)

;; Projectile
(defvar projectile-known-projects)
(defun mb-hooks--projectile-mode ()
  "My `projectile' mode hook."

  (unless projectile-known-projects
    (my/projectile-index-projects))

  (setq projectile-mode-line
        '(:eval (if (or (file-remote-p default-directory)
                        (string-match-p "/run/user/[0-9]+/gvfs/"
                                        default-directory))
                    " [?]"
                  (format " [%s]" (projectile-project-name)))))

  (projectile-register-project-type 'jhbuild
                                    (lambda () nil)
                                    "jhbuild make"
                                    "make check"
                                    "jhbuild make && jhbuild run ${PWD##*/}")

  (my/define-keys projectile-command-map
                  '(( "B"   . projectile-ibuffer)
                    ( "i"   . my/projectile-index-projects)
                    ( "I"   . projectile-invalidate-cache)
                    ( "d"   . projectile-dired)
                    ( "V"   . my/projectile-gitg)
                    ( "D"   . projectile-find-dir)))

  (control-mode-reload-bindings)

  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))

  (def-projectile-commander-method ?q
    "Go back to project selection."
    (projectile-switch-project))

  (def-projectile-commander-method ?t
    "Open a terminal in the project root."
    (ansi-term (getenv "SHELL")
               (format "*ansi-term [%s]*" (projectile-project-name)))))

(add-hook 'projectile-mode-hook #'mb-hooks--projectile-mode)

;; Python
(defvar anaconda-mode-map)
(defvar python-mode-map)
(defvar my/realgud-debugger)
(defvar yas-indent-line)
(defun mb-hooks--python-mode ()
  "My `python' mode hook."
  (setq-local fill-column 79)           ; PEP0008 says lines should be 79 chars
  (setq-local my/realgud-debugger #'realgud:ipdb)
  (setq-local company-backends '(company-anaconda))
  (anaconda-mode)
  (anaconda-eldoc-mode)
  (setq-local yas-indent-line 'fixed)
  (my/define-keys python-mode-map
                  '(( "."         . my/dot-and-complete)
                    ( "<tab>"     . my/indent-snippet-or-complete))))

(add-hook 'python-mode-hook #'mb-hooks--python-mode)

;; Anaconda
;; TODO: Figure out bindings for this
(defun mb-hooks--anaconda-mode ()
  "My `anaconda' mode hook."
  (my/define-keys anaconda-mode-map
                  '(( "C-<return>" . anaconda-mode-find-definitions)
                    ( "M-<return>" . anaconda-mode-find-assignments)
                    ( "C-z h d"    . anaconda-mode-show-doc)
                    ( "M-?"        . anaconda-mode-find-references))))

(add-hook 'anaconda-mode-hook #'mb-hooks--anaconda-mode)

;; Realgud Track
(defvar realgud-track-mode-map)
(defun mb-hooks--realgud-track-mode ()
  "My `realgud-track' mode hook."
  (my/define-keys realgud-track-mode-map
                  '(( "."      . my/dot-and-complete)
                    ( "<tab>"  . my/snippet-or-complete))))

(add-hook 'realgud-track-mode-hook #'mb-hooks--realgud-track-mode)

;; REST Client
(defvar restclient-mode-map)
(defun mb-hooks--restclient-mode ()
  "My `restclient' mode hook."
  (company-mode)
  (setq-local company-backends '((company-restclient)))
  (my/define-keys restclient-mode-map
                  '(( "<tab>" . my/snippet-or-complete))))

(add-hook 'restclient-mode-hook #'mb-hooks--restclient-mode)

;; Rust
(defvar rust-mode-map)
(defun mb-hooks--rust-mode ()
  "My `rust' mode hook."
  (racer-mode)
  (my/define-keys rust-mode-map
                  '(( "C-<return>" . racer-find-definition)
                    ( "."          . my/dot-and-complete)
                    ( ":"          . my/double-colon-and-complete))))

(add-hook 'rust-mode-hook #'mb-hooks--rust-mode)

;; Shell
(defvar term-raw-map)
(defvar yas-dont-activate)
(defun mb-hooks--term-mode ()
  "My `term' mode hook."
  (setq yas-dont-activate t)
  (my/define-keys term-raw-map
                  '(( "M-x"       . smex)
                    ( "C-y"       . my/term-paste)
                    ( "<escape>"  . ESC-prefix))))

(defun mb-hooks--term-exec ()
  "My `term' mode hook."
  (set-buffer-process-coding-system 'utf-8-unix
                                    'utf-8-unix))

(add-hook 'term-mode-hook #'mb-hooks--term-mode)
(add-hook 'term-exec-hook #'mb-hooks--term-exec)

;; Shell script
(defun mb-hooks--sh-mode ()
  "My `sh' mode hook."
  (setq-local defun-prompt-regexp
              (concat "^\\("
                      "\\(function[ \t]\\)?[ \t]*[[:alnum:]-_]+[ \t]*([ \t]*)"
                      "\\|"
                      "function[ \t]+[[:alnum:]-_]+[ \t]*\\(([ \t]*)\\)?"
                      "\\)[ \t]*"))
  (setq-local my/realgud-debugger #'realgud:bashdb)
  (setq-local company-backends '((company-shell
                                  company-keywords
                                  company-files
                                  company-dabbrev-code)))
  (sh-extra-font-lock-activate))

(add-hook 'sh-mode-hook #'mb-hooks--sh-mode)


;; Sql
(defun mb-hooks--sql-mode ()
  "My `sql' mode hook."
  (sqlup-mode))

(add-hook 'sql-mode-hook #'mb-hooks--sql-mode)


;; Vala
(defvar flycheck-checkers)
(defun mb-hooks--vala-mode ()
  "My `vala' mode hook."
  (require 'flycheck-vala)
  (add-to-list 'flycheck-checkers 'vala-valac))

(add-hook 'vala-mode-hook #'mb-hooks--prog-mode)
(add-hook 'vala-mode-hook #'mb-hooks--vala-mode)

;; Woman
(defvar woman-mode-map)
(defun mb-hooks--woman-mode ()
  "My `woman' mode hook."
  (my/remap-keys woman-mode-map
                 '(("a" . "s")
                   ("s" . "C-s")
                   ("R" . "r")
                   ("r" . "C-r"))))

(add-hook 'woman-mode-hook #'mb-hooks--woman-mode)

(provide 'mb-hooks)
;;; mb-hooks.el ends here
;;; my-lisp.el --- Some functions and macros I use. -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014 Mattias Bengtsson

;; Author: Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version: 20141020
;; Keywords: extensions, tools
;; Package-Requires: ((yasnippet "20141117.327"))
;; URL: TBA
;; Doc URL: TBA
;; Compatibility: GNU Emacs: 24.x

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;;; Note:

;;; Code:

(require 'yasnippet)

(defun my/shorten-minor-modes (modes)
  "Shorten the displayed name for MODES in the modeline."
  (dolist (mode-and-line modes)
    (let ((line (cdr mode-and-line))
          (mode (car mode-and-line)))
      (my/shorten-minor-mode mode line))))

(defun my/shorten-minor-mode (mode line)
  "Replace the displayed name for MODE by LINE."
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda () (diminish mode line)))))

(defun my/shorten-major-modes (modes)
  "Shorten the displayed name for MODES in the modeline."
  (dolist (mode-and-line modes)
    (let ((line (cdr mode-and-line))
          (mode (car mode-and-line)))
      (my/shorten-major-mode mode line))))

(defun my/shorten-major-mode (mode line)
  "Replace the displayed name for MODE by LINE."
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda () (setq-local mode-name line)))))

(defun my/auto-modes (modes)
  "Add many MODES to `auto-mode-alist'."
  (setq auto-mode-alist (append modes auto-mode-alist)))

(defun my/global-set-keys (keybindings)
  "Set a bunch of global KEYBINDINGS at the same time."
  (dolist (binding keybindings)
    (let* ((key  (car binding))
           (func (cdr binding)))
      (global-set-key (kbd key) func))))

(defun my/mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but apply FN-HEAD to CAR and FN-REST to CDR of LIST."
  (if list (cons (funcall fn-head (car list))
                 (mapcar fn-rest (cdr list)))))

(defun my/split-name (s)
  "Split S by name."
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun my/lower-camel-case (s)
  "Camel case S."
  (let ((names (my/split-name s)))
    (concat (downcase (car names))
            (mapconcat 'capitalize (cdr names) ""))))

(defun my/camel-case (s)
  "Camel case S."
  (mapconcat 'capitalize (my/split-name s) ""))

(defun my/snake-case (s)
  "Snake case S."
  (mapconcat 'downcase (my/split-name s) "_"))

(defun my/dash-case (s)
  "Dash case S."
  (mapconcat 'downcase (my/split-name s) "-"))

(defun my/toggle-programming-case (s)
  "Toggle programming style casing of S."
  (cond ((string-match-p "\\(?:[a-z]+_\\)+[a-z]+" s) (my/dash-case        s))
        ((string-match-p "\\(?:[a-z]+-\\)+[a-z]+" s) (my/camel-case       s))
        ((string-match-p "^\\(?:[A-Z][a-z]+\\)+"  s) (my/lower-camel-case s))
        (t                                           (my/snake-case       s)) ))

(defun my/preceding-char-match-p (pattern)
  "Match preceding char with PATTERN."
  (let ((str (string (preceding-char))))
    (string-match-p pattern str)))

(defun my/following-char-match-p (pattern)
  "Match following char with PATTERN."
  (let ((str (string (following-char))))
    (string-match-p pattern str)))

(defun my/define-keys (mode-map keybindings)
  "Set a bunch of MODE-MAP specific KEYBINDINGS at the same time."
  (dolist (binding keybindings)
    (let* ((key  (car binding))
           (func (cdr binding)))
      (define-key mode-map (kbd key) func))))

(defmacro my/bol-with-prefix (function)
  "Define a new function which call FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument.  The FUNCTION still receives the prefix argument."
  (let ((name (intern (format "my/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

(defun my/yas-expand ()
  "Perform a `yas-expand' but return nil on failure."
  (if (not (yas-minor-mode)) nil
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand))))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  "Use popup.el for yasnippet.  (PROMPT, CHOICES, DISPLAY-FN)."
  (require 'popup)
  (popup-menu*
   (mapcar
    (lambda (choice)
      (popup-make-item
       (or (and display-fn (funcall display-fn choice))
           choice)
       :value choice))
    choices)
   :prompt prompt
   ;; start isearch mode immediately
   :isearch t
   ))

(defun my/wrap-in-comment (string)
  "Wrap STRING inside comment."
  (format "%s%s%s" comment-start string comment-end))

(provide 'my-lisp)
;;; my-lisp.el ends here

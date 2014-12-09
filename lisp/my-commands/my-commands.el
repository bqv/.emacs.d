;;; my-commands.el --- Some commands I use.    -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014 Mattias Bengtsson

;; Author: Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version: 20141209
;; Keywords: extensions, tools
;; Package-Requires: ()
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

(require 'my-lisp)

;;;###autoload
(defun my/toggle-comment ()
  "Comments or uncomments current region or line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(defun my/rename-current-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;;###autoload
(defun my/list-installed-packages ()
  "Like `package-list-packages', but show only installed optional packages."
  (interactive)
  (package-initialize)
  (package-show-package-list
   (cl-remove-if-not (lambda (x) (and (not (package-built-in-p x))
                                      (package-installed-p x)))
                     (mapcar 'car package-archive-contents))))

;;;###autoload
(defun my/set-proxy ()
  "Automatically set HTTP proxy in Emacs based on system environment."
  (interactive)
  (if (and (getenv "HTTP_PROXY") (getenv "HTTPS_PROXY"))
      (setq-default url-proxy-services '(("http"  . (getenv "HTTP_PROXY"))
                                         ("https" . (getenv "HTTPS_PROXY"))
                                         ))))

;;;###autoload
(defun my/tab-indent-or-complete ()
  "Tab indent or complete (using `company-mode') depending on context."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (let ((old-indent (current-indentation)))
      (indent-for-tab-command)
      (if (and (= old-indent (current-indentation))
               (my/preceding-char-match-p "[a-zA-Z\-\.\>\_\/\:]")
               (null (my/yas-expand)))
          (company-complete-common)))))

;;;###autoload
(defun my/rtags-start ()
  "Start rdm if it isn't running."
  (interactive)
  (unless rtags-process (rtags-restart-process)))

;;;###autoload
(defun my/company-select-next-five ()
  "A bit more eager `company-select-next'."
  (interactive)
  (dotimes (number 5 nil) (company-select-next)))

;;;###autoload
(defun my/company-select-previous-five ()
  "A bit more eager `company-select-previous'."
  (interactive)
  (dotimes (number 5 nil) (company-select-previous)))

;;;###autoload
(defun my/insert-date (prefix)
  "Insert the current date in ISO extended format.
With PREFIX = 4, use ISO basic format.
With PREFIX = 16, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%Y%m%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))

;;;###autoload
(defun my/toggle-programming-case-word-at-point ()
  "Toggle programming style casing of word a point."
  (interactive)
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (my/toggle-programming-case txt)) )
    (if cml (progn (delete-region beg end) (insert cml))) ))

;;;###autoload
(defun my/byte-compile ()
  "Byte compile my configs."
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

;;;###autoload
(defun my/maximize ()
  "Maximize Emacs."
  (interactive)
  (when (display-graphic-p)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))))

(provide 'my-commands)
;;; my-commands.el ends here

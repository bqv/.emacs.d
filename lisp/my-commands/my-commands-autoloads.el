;;; my-commands-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "my-commands" "my-commands.el" (21638 35172
;;;;;;  553962 131000))
;;; Generated autoloads from my-commands.el

(autoload 'my/toggle-comment "my-commands" "\
Comments or uncomments current region or line.

\(fn)" t nil)

(autoload 'my/rename-current-buffer-and-file "my-commands" "\
Renames current buffer and file it is visiting.

\(fn)" t nil)

(autoload 'my/list-installed-packages "my-commands" "\
Like `package-list-packages', but show only installed optional packages.

\(fn)" t nil)

(autoload 'my/set-proxy "my-commands" "\
Automatically set HTTP proxy in Emacs based on system environment.

\(fn)" t nil)

(autoload 'my/tab-indent-or-complete "my-commands" "\
Tab indent or complete (using `company-mode') depending on context.

\(fn)" t nil)

(autoload 'my/rtags-start "my-commands" "\
Start rdm if it isn't running.

\(fn)" t nil)

(autoload 'my/company-select-next-five "my-commands" "\
A bit more eager `company-select-next'.

\(fn)" t nil)

(autoload 'my/company-select-previous-five "my-commands" "\
A bit more eager `company-select-previous'.

\(fn)" t nil)

(autoload 'my/insert-date "my-commands" "\
Insert the current date in ISO extended format.
With PREFIX = 4, use ISO basic format.
With PREFIX = 16, write out the day and month name.

\(fn PREFIX)" t nil)

(autoload 'my/toggle-programming-case-word-at-point "my-commands" "\
Toggle programming style casing of word a point.

\(fn)" t nil)

(autoload 'my/byte-compile "my-commands" "\
Byte compile my configs.

\(fn)" t nil)

(autoload 'my/maximize "my-commands" "\
Maximize Emacs.

\(fn)" t nil)

;;;***

(provide 'my-commands-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-commands-autoloads.el ends here

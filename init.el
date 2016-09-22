(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun package-conditional-install (package-name)
  "Install a package if it is not present"
  (unless (package-installed-p package-name)
    (package-refresh-contents) (package-install package-name)))

(defun packages-conditional-install (packages)
  "Install list of packages if not present"
  (when packages
    (package-conditional-install (car packages))
    (packages-conditional-install (cdr packages))))

(packages-conditional-install '(auto-package-update key-chord keyfreq eno company nyan-mode exec-path-from-shell elmacro yasnippet))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(auto-package-update-maybe)
(key-chord-mode 1)

(require 'yasnippet)


(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/my_snippets"
	))

(yas-global-mode 1)

;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; mac specific settings
(when (eq system-type 'darwin) 
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  )

(require 'keyfreq)
(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        previous-line
        next-line))
(nyan-mode 1)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(global-company-mode)

(load "~/.emacs.d/configs/ui")
(load "~/.emacs.d/configs/editing")
(load "~/.emacs.d/configs/programming")
(load "~/.emacs.d/configs/git")
(load "~/.emacs.d/configs/windows")
(load "~/.emacs.d/configs/helm")

;; (load "~/.emacs.d/configs/scala")
(load "~/.emacs.d/configs/erlang")
(load "~/.emacs.d/configs/other")
(load "~/.emacs.d/configs/org")
(load "~/.emacs.d/configs/greek")

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

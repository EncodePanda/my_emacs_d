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

(setq prettify-symbols-alist
      '(
	("lambda" . 955)
	))

(packages-conditional-install '(auto-package-update key-chord keyfreq eno nyan-mode))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(auto-package-update-maybe)
(key-chord-mode 1)

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

(load "~/.emacs.d/configs/ui")
(load "~/.emacs.d/configs/editing")
(load "~/.emacs.d/configs/git")
(load "~/.emacs.d/configs/windows")
(load "~/.emacs.d/configs/helm")
(load "~/.emacs.d/configs/programming")
(load "~/.emacs.d/configs/scala")
(load "~/.emacs.d/configs/erlang")
(load "~/.emacs.d/configs/other")
(load "~/.emacs.d/configs/org")

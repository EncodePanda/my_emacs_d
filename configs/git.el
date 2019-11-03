(packages-conditional-install '(magit))

(global-set-key (kbd "C-x G") 'magit-status)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; (git-gutter:update-interval 2)
;; (global-git-gutter-mode +1)

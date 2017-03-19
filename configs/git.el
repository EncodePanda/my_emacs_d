(packages-conditional-install '(magit))

(global-set-key (kbd "C-x G") 'magit-status)
;; (git-gutter:update-interval 2)
;; (global-git-gutter-mode +1)

(global-set-key (kbd "C-x N") 'magit-next-commit)

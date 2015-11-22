(packages-conditional-install '(magit git-gutter))

(global-set-key (kbd "C-x G") 'magit-status)
(git-gutter:update-interval 2)
(global-git-gutter-mode +1)

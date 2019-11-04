(packages-conditional-install '(neotree))

;; (require 'moe-theme)
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)


(global-set-key (kbd "M-o M-t t") 'neotree-toggle)
(global-set-key (kbd "M-o M-t f") 'neotree-find)

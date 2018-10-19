(packages-conditional-install '(neotree monokai-theme moe-theme))

(require 'moe-theme)
(load-theme 'moe-dark t)

(tool-bar-mode 0) 
(menu-bar-mode 0)
(toggle-frame-fullscreen) 
(scroll-bar-mode 0)

(fset `yes-or-no-p `y-or-n-p)

(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)

(global-set-key (kbd "C-=") 'zoom-frm-in)
(global-set-key (kbd "C--") 'zoom-frm-out)

(global-set-key (kbd "M-o M-t t") 'neotree-toggle)
(global-set-key (kbd "M-o M-t f") 'neotree-find)

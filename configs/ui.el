(packages-conditional-install '(zoom-frm neotree monokai-theme))

(load-theme 'monokai t)

(desktop-save-mode 1)
(tool-bar-mode 0) 
(menu-bar-mode 0)
(toggle-frame-fullscreen) 
(scroll-bar-mode 0)

(fset `yes-or-no-p `y-or-n-p)

(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)

(global-set-key (kbd "C-+") 'zoom-frm-in)
(global-set-key (kbd "C-_") 'zoom-frm-out)

(global-set-key [f8] 'neotree-toggle)

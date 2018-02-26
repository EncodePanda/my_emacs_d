(packages-conditional-install '(smartparens projectile recentf ag helm-ag helm helm-projectile))

(require 'recentf)
(require 'projectile)
(require 'helm-projectile)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

(global-set-key (kbd "C-c F") 'helm-do-ag-project-root)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-c C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x r l") 'helm-bookmarks)
(key-chord-define-global "fm" 'helm-mini)
(key-chord-define-global "qw" 'helm-do-ag-this-file)
(key-chord-define-global "cm" 'helm-projectile-switch-project)
(key-chord-define-global "pf" 'helm-projectile-find-file)
(key-chord-define-global "gm" 'helm-do-ag-project-root)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

(show-paren-mode 1)
(column-number-mode 1)
(smartparens-global-mode 1)
(projectile-global-mode 1)

(global-linum-mode 1)

(packages-conditional-install '(ace-window zoom-window))

(global-set-key (kbd "C-c \\") 'ace-window)
(key-chord-define-global "sw" 'ace-swap-window)
(key-chord-define-global "mw" 'ace-maximize-window)
(key-chord-define-global "zw" 'zoom-window-zoom)


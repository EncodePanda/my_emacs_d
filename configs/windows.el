(packages-conditional-install '(ace-window zoom-window))

(global-set-key (kbd "C-c \\") 'ace-window)
(key-chord-define-global "gh" 'ace-window)

(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

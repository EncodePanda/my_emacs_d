(packages-conditional-install '(smartparens projectile))

(show-paren-mode 1)
(column-number-mode 1)
(smartparens-global-mode 1)
(projectile-global-mode 1)

(key-chord-define-global "q[" 'sp-backward-sexp)
(key-chord-define-global "q]" 'sp-forward-sexp)

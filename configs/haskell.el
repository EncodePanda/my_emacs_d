(packages-conditional-install '(haskell-mode intero hasky-stack))
(require 'hasky-stack)

(custom-set-variables
 '(haskell-stylish-on-save t))

(add-hook 'haskell-mode-hook 'intero-mode)

;;(require 'flymake-hlint) ;; not needed if installed via package
;;(add-hook 'haskell-mode-hook 'flymake-hlint-load)

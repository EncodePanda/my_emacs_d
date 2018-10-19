(packages-conditional-install '(sbt-mode scala-mode))
;; bug in sbt-mode fixed 
(setq sbt:prefer-nested-projects t)

(add-hook 'sbt-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  `((,(expand-file-name (directory-file-name default-directory)) . ?⌂)
                    (,(expand-file-name "~") . ?~)))
            (prettify-symbols-mode t)))

(require 'sbt-mode)
(setq compilation-auto-jump-to-first-error t)
(global-set-key (kbd "C-c s") 'sbt-hydra)


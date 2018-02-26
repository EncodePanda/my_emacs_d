;; All other packages
(packages-conditional-install '(keyfreq nyan-mode company eno elmacro auto-highlight-symbol hl-todo wttrin))

;; collects stats of keyusage
(require 'keyfreq)
(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        previous-line
        next-line))

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; nyancut flying around :)
(nyan-mode 1)

;; copy buffer's path to clipboard
(defun put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; company modes provides magic text completion
(global-company-mode)
(add-hook 'haskell-mode-hook 'intero-mode)

;; (add-to-list 'company-backends 'company-cabal)
;; (add-to-list 'company-backends 'company-dabbrev-code) 
;; (add-to-list 'company-backends 'company-yasnippet)
;; (add-to-list 'company-backends 'company-files)

(setq org-reveal-root "file:///Users/rabbit/projects/reveal.js/js/reveal.js")

;; jumping betweetn TODOs
(defhydra hydra-todo (:pre
                  (hl-todo-mode 1)
              :post
             (hl-todo-mode -1))
  "Todo"
  ("n" hl-todo-next "Next")
  ("p" hl-todo-previous "Previous")
  ("o" hl-todo-occur "Occur")
  ("q" nil "Quit" :color blue :exit t))

(setq wttrin-default-cities '("Wroclaw"))
(setq wttrin-default-accept-language '("Accept-Language" . "en-US"))

(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive) 
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))

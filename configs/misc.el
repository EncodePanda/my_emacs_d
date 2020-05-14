;; All other packages
(packages-conditional-install '(keyfreq company eno elmacro auto-highlight-symbol hl-todo wttrin))

;; collects stats of keyusage
(require 'keyfreq)
(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        previous-line
        next-line))

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
;; TODO

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



;; jumping betweetn TODO
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

;; All other packages
(packages-conditional-install '(keyfreq nyan-mode company eno elmacro))

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

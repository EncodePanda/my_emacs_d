(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-default-notes-file (concat org-directory "/captured.org"))

(setq org-refile-targets '(
   (nil :maxlevel . 2)             ; refile to headings in the current buffer
   (org-agenda-files :maxlevel . 2) ; refile to any of these files
   ))

(setq org-agenda-files '("~/org"))

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/capture.org" "Tasks")
    "* TODO %?\n  %i\n  %a")
   ("j" "Pyrofex JIRA issues wannabes" entry (file+headline "~/org/pyrofex-jira.org" "JIRA issues")
        "* TODO %?\n  %i\n  %a")
   ("p" "Pyrofex work" entry (file+datetree "~/org/pyrofex-log.org")
        "* %? %U")))

;; (setq org-columns-default-format
      ;; "%25ITEM %TODO %SCHEDULED %TAGS")

;; (add-to-list 'org-agenda-custom-commands
             ;; '("t" "List of all TODO entries"
               ;; alltodo ""
               ;; ((org-agenda-view-columns-initially t))
               ;; ))

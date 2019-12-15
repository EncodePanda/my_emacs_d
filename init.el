;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add package repositories.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Install use-package
;;
;; With this package all other package will be installed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Quelpa
;;
;; Quelpa allows installing packages directly from git repositories
;; like github or gitlab
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package quelpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialize Dashboard
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-banner-logo-title "Welcome back, Rabbit")
(setq dashboard-startup-banner "~/.emacs.d/logo.png")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hydras
;;
;; Majore Mode Hydra is needed to be installed first so that we can later on
;; define hydras per major mode (e.g for haskell, orm-mode)
;;
;; Key bind to meta-space which under this configuration is cmd-space. This
;; requires user to reconfigure their Mac/OSX keybindg for spotlight search.
;; I've assgined that to cmd+enter on my local OSX.
;;
;; Also by installing major-mode-hydra we get pretty-hydra which will also be
;; used below
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package major-mode-hydra
  :bind
  ("C-M-SPC" . major-mode-hydra))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Which key
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq which-key-enable-extended-define-key t)
(use-package which-key)
(which-key-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UI/UX custimizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; small UI improvements
(tool-bar-mode 0)           ;; no tool bar
(menu-bar-mode 0)           ;; no menu bar
(toggle-frame-fullscreen)   ;; start with fullscreen
(scroll-bar-mode 0)         ;; no scrollbar
(show-paren-mode 1)         ;; highlight matchin paranthesis
(column-number-mode 1)      ;; show column number in minibuffer
(global-linum-mode 1)       ;; show line numbers in gutter

;; answer questions with y/n (instead of yes/no)
(fset `yes-or-no-p `y-or-n-p)

;; load theme
(use-package moe-theme
  :ensure t)
(load-theme 'moe-dark t)

;; quick-switch-themes allows to quickly toggle between defined themes
(defvar quick-switch-themes
  (let ((themes-list (list 'moe-dark
                           'moe-light)))
    (nconc themes-list themes-list)))
(defun quick-switch-themes* ()
  (interactive)
  (if-let* ((next-theme (cadr quick-switch-themes)))
      (progn (when-let* ((current-theme (car quick-switch-themes)))
               (disable-theme (car quick-switch-themes)))
             (load-theme next-theme t)
             (message "Loaded theme: %s" next-theme))
    ;; Always have the dark mode-line theme
    (mapc #'disable-theme (delq 'smart-mode-line-dark custom-enabled-themes)))
  (setq quick-switch-themes (cdr quick-switch-themes)))

;; zoom in/out a.k.a presentation mode
(load "~/.emacs.d/configs/frame-fns.el")
(load "~/.emacs.d/configs/frame-cmds.el")
(load "~/.emacs.d/configs/zoom-frm.el")
(global-set-key (kbd "C-=") 'zoom-frm-in)
(global-set-key (kbd "C--") 'zoom-frm-out)

;; show buffers that were opened recently
;; helpful if you just restarted your Emacs
(use-package recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mac / OSX specific configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; needed for PATH recognition
(use-package exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-initialize)

;; bind meta and super to cmd and option
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  )

;; uses oxs notifier as default TODO not sure if its working ...
(setq alert-default-style 'osx-notifier)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eyebrowse)
(eyebrowse-mode t)

(use-package dumb-jump)

(use-package duplicate-thing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit is a powerful interface to git
(use-package magit
  :bind ("C-x G" . magit-status))

;; time machine allows inspecting changes on a single file
;; we can move back and forth to see the progress on a given file
(use-package git-timemachine)

;; By default ediff pops out a separate frame for navigation during the difff.
;; This change below keeps the ediff in the same frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; this will refresh buffer if file changed on a disk e.g loaded new branch
(global-auto-revert-mode t)

;; show which lines were added/modfied/removed
;; git-gutter-fringe+ words perfectly with linum-mode but only in
;; graphical environment (this will not work in terminal)
(use-package git-gutter-fringe+)
(global-git-gutter+-mode t)
;; refresh gutter when staged by magit
(defun my-refresh-visible-git-gutter-buffers ()
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when (and git-gutter+-mode (get-buffer-window buff))
        (git-gutter+-mode t)))))

(add-hook 'magit-post-refresh-hook
          #'my-refresh-visible-git-gutter-buffers)

(pretty-hydra-define hydra-git (:foreign-keys warn :title "Git" :quit-key "q" :exit t)
  ("Magit"
   (("o" magit "open")
    ("b" magit-blame "blame")
   )

   "Other"
   (("t" git-timemachine "time machine")
    ("s" git-gutter+-show-hunk "show hunk" :exit nil)
    ("k" git-gutter+-previous-hunk "previous hunk" :exit nil)
    ("j" git-gutter+-next-hunk "previous hunk" :exit nil)
    ("x" git-gutter+-revert-hunk "kill hunk" :exit nil)
   )
   ))
(global-set-key (kbd "C-c g") 'hydra-git/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Company mode
;;
;; This mode enables completion, supports many backends
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable company mode for all files
(use-package company
  :config
  (add-to-list 'company-backends 'company-etags)
)
(global-company-mode t)

;; "aggressive" completion (no delays, quick feedback)
(setq company-idle-delay 0
      company-echo-delay 0
      company-dabbrev-downcase nil
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org)
;; org bullets for nicer rendering of org files
(use-package org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; autolist changes behaviour to more familiar one from non-programming editors
;; e.g hiting RET at the end of a list creats new entry in that list
(use-package org-autolist
  :ensure)
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

;; bindings, todo use use-package
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-default-notes-file (concat org-directory "/captured.org"))

(setq org-refile-targets '(
   (nil :maxlevel . 2)             ; refile to headings in the current buffer
   (org-agenda-files :maxlevel . 2) ; refile to any of these files
   ))

(setq org-agenda-files (directory-files-recursively "~/org/" "\.org$"))

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/capture.org" "Tasks")
    "* TODO %?\n  %i")
   ("j" "JIRA" entry (file+headline "~/org/jira.org" "JIRA issues")
        "* TODO %?\n  %i\n  %a")
   ("w" "Work log" entry (file+datetree "~/org/work-log.org")
        "* %? %U")))

(defun markdown-convert-buffer-to-org ()
    "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
    (interactive)
    (shell-command-on-region (point-min) (point-max)
                             (format "pandoc -f markdown -t org -o %s"
                                     (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

;; add to eneeded to produce latex/pdf for org more
;; TODO document software hat needs to be installed
(if (eq window-system 'mac)
   (add-to-list 'exec-path "/usr/local/texlive/2019/bin/x86_64-darwin")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Haskell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package haskell-mode)
(load "~/.emacs.d/configs/ghcid.el")
(use-package haskell-snippets)
(require 'haskell-snippets)
(setq haskell-import-mapping
      '(("Data.Attoparsec.Char8" . "import qualified Data.Attoparsec.Char8 as Atto8
")
        ("Data.Text" . "import qualified Data.Text as T
import Data.Text (Text)
")
        ("Data.Text.Encoding" . "import qualified Data.Text.Encoding as T
")
        ("Data.Text.Lazy.Encoding" . "import qualified Data.Text.Lazy.Encoding as LT
")
        ("Data.Text.Lazy" . "import qualified Data.Text.Lazy as LT
")
        ("Data.Text.IO" . "import qualified Data.Text.IO as T
")
        ("Data.Text.Lazy.IO" . "import qualified Data.Text.IO as LT
")
        ("Data.ByteString" . "import qualified Data.ByteString as S
import Data.ByteString (ByteString)
")
        ("Data.ByteString.Char8" . "import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)
")
        ("Data.ByteString.Lazy" . "import qualified Data.ByteString.Lazy as L
")
        ("Data.ByteString.Lazy.Builder" . "import qualified Data.ByteString.Builder as SB
")
        ("Data.ByteString.Builder" . "import qualified Data.ByteString.Builder as SB
")
        ("Data.ByteString.Lazy.Char8" . "import qualified Data.ByteString.Lazy.Char8 as L8
")
        ("Data.Map" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
")
        ("Data.HashMap" . "import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
")
        ("Data.IntMap" . "import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
")
        ("Data.StrMap" . "import Data.StrMap as StrMap
import Data.StrMap (StrMap)
")
        ("Data.Map.Strict" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
")
        ("Data.Set" . "import qualified Data.Set as Set
import Data.Set (Set)
")
        ("Data.Vector" . "import qualified Data.Vector as V
import Data.Vector (Vector)
")
        ("Data.Vector.Storable" . "import qualified Data.Vector.Storable as SV
import Data.Vector (Vector)
")
        ("Data.List.NonEmpty" . "import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
")
        ("Data.Conduit.List" . "import qualified Data.Conduit.List as CL
")
        ("Data.Conduit.Binary" . "import qualified Data.Conduit.Binary as CB
")
        ("Data.Sequence" . "import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
")))

(setq haskell-imports-helm-source
      `((name . "*helm* Insert Haskell import")
        (candidates . ,haskell-import-mapping)
        (action . (lambda (candidate)
                    (helm-marked-candidates)))))

(defun haskell-imports-helm ()
  (interactive)
  (insert
   (mapconcat 'identity
              (helm :sources '(haskell-imports-helm-source))
              ",")))

(major-mode-hydra-define haskell-mode nil
  ("Navigation"
   (("o" haskell-navigate-imports  "imports"))
   "Editing"
   (("i" haskell-imports-helm  "imports")
    ("y" yyas-describe-table "snippets")
    ("g" ghcid "ghcid")
    )
   "Documentation"
   (("h" hoogle "hoogle"))))

;;(require 'flymake-hlint) ;; not needed if installed via package
;;(add-hook 'haskell-mode-hook 'flymake-hlint-load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configuration for editing ELisp code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Evil-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil)
(use-package evil-leader)
(use-package evil-mc)

(require 'evil)
(require 'evil-leader)

(global-evil-mc-mode  1)

(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)

(use-package evil-magit)
(use-package evil-surround)
(global-evil-surround-mode 1)
(use-package evil-nerd-commenter)

(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "y" 'helm-show-kill-ring
  "u" 'undo-tree-visualize
  "b" 'switch-to-buffer
  "ww" 'ace-window
  "w1" 'eyebrowse-switch-to-window-config-1
  "w2" 'eyebrowse-switch-to-window-config-2
  "w3" 'eyebrowse-switch-to-window-config-3
  "wh" 'split-window-horizontally
  "wv" 'split-window-vertically
  "w4" 'eyebrowse-switch-to-window-config-4
  "k" 'kill-buffer
  "g" 'hydra-git/body
  "m" 'major-mode-hydra
  "n" 'eno-word-goto
  "jj" 'dumb-jump-go
  "jb" 'dumb-jump-back
)
(evil-mode 1)
;;(key-chord-defne evil-insert-state-map  "C-SPC" 'evil-normal-state)
;;(global-set-key (kbd "C-c l") 'lsp-ui-peek-find-definitions)
;;(global-set-key (kbd "C-c h") 'lsp-ui-peek-jump-backward)

;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)
(provide 'init-evil)
(load "~/.emacs.d/configs/install_first")
(load "~/.emacs.d/configs/hydras")
(load "~/.emacs.d/configs/yasnippet")
(load "~/.emacs.d/configs/scala")
(load "~/.emacs.d/configs/misc")
(load "~/.emacs.d/configs/ui")
(load "~/.emacs.d/configs/editing")
(load "~/.emacs.d/configs/project")
(load "~/.emacs.d/configs/windows")
(load "~/.emacs.d/configs/erlang")
(load "~/.emacs.d/configs/other")
(load "~/.emacs.d/configs/org")
(load "~/.emacs.d/configs/greek")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (haskell-snippets shm zoom-window yasnippet xah-fly-keys wttrin which-key use-package string-edit smartparens scala-mode sbt-mode quelpa protobuf-mode pandoc-mode org-bullets org-autolist nyan-mode neotree multiple-cursors monokai-theme moe-theme markdown-mode major-mode-hydra keyfreq key-chord intero hl-todo highlight-symbol helm-swoop helm-projectile helm-etags-plus helm-ag hasky-stack git-timemachine git-gutter-fringe+ eyebrowse exec-path-from-shell evil-surround evil-nerd-commenter evil-mc evil-magit evil-leader etags-select erlang eno encourage-mode elmacro eldoro duplicate-thing dumb-jump dashboard csv-mode code-library auto-package-update auto-highlight-symbol annoying-arrows-mode ag ace-window))))

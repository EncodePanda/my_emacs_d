;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add package repositories.
;;
;; First we need to tell Emacs to use 'package' module. With it, we will be able
;; to fetch additional modules from the Internet.
;; The 'package-archives' holds the modules' repositories
;;
;; You can M-x list-packages to see all the available packages that are
;; available
;;
;; Please note that I will be using words 'package' and 'mode' interchangeably
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-check-signature nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Install use-package
;;
;; We will use this package to install all other packages
;;
;; When using use-package:
;; * Use the :init keyword to execute code before a package is loaded.
;; * Use the :config keyword to execute code after a package is loaded
;; * Use the :bind keyword to bind primary commands within that module
;; * Use the :hook keyword to add functions onto package hooks
;; * Use the :after to configure a package after another has been loaded
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
;; when installing package, it will be always downloaded automatically from
;; repository if is not available locally
(setq use-package-always-ensure t)

;; This allows you to ensure that specific binary is globally available on your
;; system. If not, it will install it automatically.
;; See https://github.com/jwiegley/use-package#use-package-ensure-system-package
;; for details
(use-package use-package-ensure-system-package)

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

;; osx-lib gives nice adapters to OSX functionality
(use-package osx-lib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hydras - used to tie related commands into a family of short bindings with
;; a common prefix. Similar to leader key but more powerful
;;
;; Majore Mode Hydra is needed to be installed first so that we can later on
;; define hydras per major mode (e.g for haskell, orm-mode)
;;
;; Also by installing major-mode-hydra we get pretty-hydra which will also be
;; used below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package major-mode-hydra
  :bind     ("C-M-SPC" . major-mode-hydra))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Evil & friends
;;
;; Provides Vim like features: normal mode, insert mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key-chord maps a pair of simultaneously pressed keys (or the same key being
;; pressed twice in quick succession) to a command. Such bindings are called
;; "key chords". allows
(use-package key-chord
  :config   (key-chord-mode 1))

(use-package evil
  :init     (setq evil-want-keybinding nil)
  :config   (evil-mode)
            ;; visually differentiate between normal and insert mode
            (setq evil-normal-state-cursor '(box "yellow"))
            (setq evil-insert-state-cursor '(bar "white")))

;; This s a collection of Evil bindings for the parts of Emacs that Evil does
;; not cover properly by default, such as help-mode, M-x calendar, Eshell and
;; more.
(use-package evil-collection
  :after    evil
  :custom   ;; use evil in minibuffer
            (evil-collection-setup-minibuffer t)
  :config   ;; use fj key-chord to enter normal state
            (key-chord-define-global "fj" 'evil-normal-state)
	    ;; init evil-collection globally
            (evil-collection-init))

;; leader key provides the <leader> feature from Vim - an easy way to bind keys
;; under a variable prefix key (here the space-bar)
(use-package evil-leader
  :after    evil-collection
  :config   (global-evil-leader-mode)
            (evil-leader/set-leader "<SPC>"))

;; visualize evil actions
(use-package evil-goggles
  :after    evil
  :config   (evil-goggles-mode)
            (custom-set-faces
             '(evil-goggles-delete-face ((t (:inherit 'shadow))))
             '(evil-goggles-paste-face ((t (:inherit 'lazy-highlight))))
             '(evil-goggles-yank-face ((t (:inherit 'isearch-fail))))))


;; Call M-x evil-tutor-start to start learning evil mode
(use-package evil-tutor
  :after    evil)

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
(show-paren-mode 1)         ;; highlight matchin parenthesis
(column-number-mode 1)      ;; show column number in minibuffer
(global-linum-mode 1)       ;; line numbers

;; keep text to 80 columns
(use-package display-fill-column-indicator
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  (org-mode . display-fill-column-indicator-mode)
  :config
  (setq-default fill-column 80))

;; deeply nested delimiters get different colors
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (progn
    (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode t)))))

;; clm/open-command-log-buffer opens small buffer that shows all the keystrokes
;; and functions used while oparating Emacs
(use-package command-log-mode
  :config
  (global-command-log-mode)
)

;; answer questions with y/n (instead of yes/no)
(fset `yes-or-no-p `y-or-n-p)

;; load theme
(use-package moe-theme
  :config
  (setq moe-dark-comment-delimiter          -moe-dark-doc        )
  (setq moe-dark-comment                    -moe-dark-doc        )
)
(load-theme 'moe-dark t)

;; allows quick theme change
(use-package helm-themes)

;; nyancut flying around :)
(use-package nyan-mode)
(nyan-mode 1)

;; encourage your work each time you save a buffer :)
(use-package encourage-mode)
(encourage-mode 1)
(setq encourage-encouragements
  (nconc encourage-encouragements
    '("Good job"
      "Great initiative"
      "Nice work")))

;; zoom in/out a.k.a presentation mode
(load "~/.emacs.d/configs/frame-fns.el")
(load "~/.emacs.d/configs/frame-cmds.el")
(load "~/.emacs.d/configs/zoom-frm.el")

;; show buffers that were opened recently
;; helpful if you just restarted your Emacs
(use-package recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

;; Hydra for UI/UX related stuff
(pretty-hydra-define hydra-uiux (:foreign-keys warn :title "UI/UX" :quit-key "q")
  ("Zoom"
   (("i" zoom-frm-in "(+) in")
    ("o" zoom-frm-out "(-) out")
   )
   "Volume"
   (("m" osx-lib-mute-volume "mute")
    ("M" osx-lib-unmute-volume "unmute")
   )

   "Theme"
   (("t" helm-themes "theme switch")
   )
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm - incremental completion and selection narrowing framework
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm)
(global-set-key (kbd "M-x") 'helm-M-x)            ;; replace default M-x
(global-set-key (kbd "C-x C-f") 'helm-find-files) ;; replace default find file
(global-set-key (kbd "C-s") 'helm-occur)          ;; replace default search

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; World clock
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run with display-time-world
(setq display-time-world-list
      '(("Europe/London"                    "London - UK")
        ("Europe/Warsaw"                    "Warsaw - PL")
        ("Australia/Canberra"               "Canberra - AU")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Project exploration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile)      ;; project interaction library
(projectile-global-mode 1)
(use-package helm-projectile) ;; integrate helm with projectile
(use-package ag)              ;; search using ag (silversearch)
(use-package helm-ag)         ;; helm integration with ag

(evil-leader/set-key
  "pb" 'helm-mini                         ;; all buffers via helm
  "pp" 'helm-projectile-find-file         ;; all project files
  "pf" 'helm-projectile-recentf           ;; project recently opened buffers
  "pr" 'projectile-replace                ;; replace occurances in whole project
  "pg" 'helm-do-ag-project-root           ;; grap content in the project
  "ps" 'helm-projectile-switch-project     ;; switch to known project
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dump-jump is like having etags without a need to generate tags in the first
;; place. dumb-jump works with different search engines, for me
;; ag (silversearch) works best
(use-package dumb-jump
  :config (setq dumb-jump-force-searcher 'ag)
)
(evil-leader/set-key
  "jj" 'dumb-jump-go
  "jb" 'dumb-jump-back
  "jw" 'dumb-jump-go-prompt
  )
;; eno and avy are nice combo to jump between places in visible part of buffer
(use-package eno)
(use-package avy)
(evil-leader/set-key
  "nn" 'eno-word-goto
  "nl" 'avy-goto-line
  "nc" 'goto-last-change
  "nw" 'evil-avy-goto-char-timer
  )
;; jump to last change
(use-package goto-chg)
(evil-leader/set-key
  "nc" 'goto-last-change
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bookmakrs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-leader/set-key
  "bs" 'helm-bookmarks     ;; search bookmarks
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Editing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visual undo (with branches)
(use-package undo-tree)
(global-undo-tree-mode 1)
;; fine-graned undo with evil mode, without this undo has not friendly
;; experience where it undo things in chunks
(setq evil-want-fine-undo t)
(evil-leader/set-key
  "y" 'helm-show-kill-ring
  "uu" 'undo-tree-visualize
  "us" 'undo-tree-save-state-to-register
  "ur" 'undo-tree-restore-state-to-register
)
;; contextual selection
(use-package expand-region)
;; this unbinds evil C-e
(eval-after-load "evil-maps" (define-key evil-motion-state-map "\C-e" nil))
;; this bind C-e to expand-region
(global-set-key (kbd "C-e") 'er/expand-region)

;; auto highlight symbols at point
(use-package auto-highlight-symbol)

;; edit multiple regions simultaneously in a buffer or a region
(use-package iedit)
(use-package evil-iedit-state)
(define-key evil-normal-state-map (kbd "ge") 'evil-iedit-state/iedit-mode)

;; folds text
(use-package vimish-fold)
(vimish-fold-global-mode 1)
(evil-leader/set-key
  "vf" 'vimish-fold
  "vd" 'vimish-fold-delete
  "vD" 'vimish-fold-delete-all
  "vu" 'vimish-fold-unfold
  "vU" 'vimish-fold-unfold-all
  "vt" 'vimish-fold-toggle
  "vT" 'vimish-fold-toggle-all
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialize Dashboard
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-banner-logo-title "Welcome back, Encode Panda")
(setq dashboard-startup-banner "~/.emacs.d/logo.png")
(push 'dashboard-mode evil-insert-state-modes)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq which-key-enable-extended-define-key t) ;; for custom name replacement
(use-package which-key)                       ;; install package
(which-key-mode)                              ;; enable globally


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eyebrowse)                   ;; like linux multiple desktop support
(eyebrowse-mode t)
(use-package ace-window)                  ;; window management made easy
(winner-mode 1)                           ;; undo/redo for windows management

(global-set-key (kbd "M-]") 'next-buffer)     ;; to buffer on the left
(global-set-key (kbd "M-[") 'previous-buffer) ;; to buffer on the right
(evil-leader/set-key
  "ww" 'ace-window
  "ws" 'ace-swap-window
  "w1" 'eyebrowse-switch-to-window-config-1
  "w2" 'eyebrowse-switch-to-window-config-2
  "w3" 'eyebrowse-switch-to-window-config-3
  "w4" 'eyebrowse-switch-to-window-config-4
  "w5" 'eyebrowse-switch-to-window-config-5
  "w6" 'eyebrowse-switch-to-window-config-6
  "wv" 'split-window-horizontally
  "wh" 'split-window-vertically
  "wb" 'balance-windows
  "wx" 'ace-delete-window
  "wk" 'delete-other-windows         ;; deletes other windows, no questions asked
  "wu" 'winner-undo                  ;; undo any window change
  "wU" 'winner-redo                  ;; redo any window change
  "k" 'kill-buffer
)
;; set jumping keys to home row (it's digits by default)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; back-up jump (if evil not available)
(global-set-key (kbd "C-c \\") 'ace-window)

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

;; inline git blame
(use-package git-messenger)
(setq git-messenger:show-detail t) ;; more details in popup (author, date etc.)

(pretty-hydra-define hydra-git (:foreign-keys warn :title "Git" :quit-key "q" :exit t)
  ("Git"
   (("o" magit "open")
    ("b" magit-blame "blame")
    ("p" git-messenger:popup-message "blame line")
   )
   "Hunks"
   (("w" git-gutter+-show-hunk "show hunk" :exit nil)
    ("k" git-gutter+-previous-hunk "previous hunk" :exit nil)
    ("j" git-gutter+-next-hunk "previous hunk" :exit nil)
    ("x" git-gutter+-revert-hunk "kill hunk" :exit nil)
    ("s" git-gutter+-stage-hunks "stage hunk" :exit nil)
   )
   "Log"
   (("t" git-timemachine "time machine")
    ("a" magit-log-all "log all")
    ("f" magit-log-buffer-file "log file")
   )
   ))
(global-set-key (kbd "C-c g") 'hydra-git/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete and kill buffer
(defun delete-file-visited-by-buffer (buffername)
  "Delete the file visited by the buffer named BUFFERNAME."
  (interactive "b")
  (let* ((buffer (get-buffer buffername))
         (filename (buffer-file-name buffer)))
    (when filename
      (delete-file filename)
      (kill-buffer-ask buffer))))
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
;; hydra for file manipulation
(pretty-hydra-define hydra-file(:foreign-keys warn :title "File" :quit-key "q" :exit t)
  ("File"
   (("x" delete-file-visited-by-buffer "delete")
    ("p" put-file-name-on-clipboard "path to clipboard")
   )
   ))
(evil-leader/set-key
  "g" 'hydra-git/body
  "x" 'hydra-uiux/body
)
(define-key evil-normal-state-map (kbd "gf") 'hydra-file/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Company mode
;;
;; This mode enables completion, supports many backends
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable company mode for all files
(use-package company-ghc)
(use-package company
  :init
  (setq company-backends '((company-ghc company-files company-keywords company-capf company-dabbrev-code company-dabbrev company-ispell)))
  :config
  (global-company-mode)

;; (add-to-list 'company-backends 'company-yasnippet)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  :bind ("C-<tab>" . 'company-complete-common-or-cycle)
)
;; "aggressive" completion (no delays, quick feedback)
(setq company-idle-delay 0
      company-echo-delay 0
      company-dabbrev-downcase nil
      company-minimum-prefix-length 4
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some handful set of modes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode)
(use-package csv-mode)
(use-package yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spell-checkng
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell)
(use-package flyspell-correct-helm
   :bind ("C-M-;" . flyspell-correct-wrapper)
   :defer t
   :init
   (progn
     (add-hook 'prog-mode-hook 'flyspell-prog-mode)
     (add-hook 'text-mode-hook 'flyspell-mode)
   (setq flyspell-correct-interface #'flyspell-correct-helm)))
;; TODO https://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :init
  (setq org-agenda-files (directory-files-recursively "~/org/" "\.org$"))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)
  (setq org-default-notes-file (concat org-directory "/captured.org"))
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
)

(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)

;; org bullets for nicer rendering of org files
(use-package org-bullets
  :after org
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
)

;; autolist changes behaviour to more familiar one from non-programming editors
;; e.g hiting RET at the end of a list creats new entry in that list
(use-package org-autolist
  :after org
)

;; evil bindings
;; (use-package evil-org
;;   :ensure t
;;   :after org
;;   :hook (org-mode . (lambda () evil-org-mode))
;;   :config
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))
;; bindings, todo use use-package

(setq org-refile-targets '(
   (nil :maxlevel . 2)             ; refile to headings in the current buffer
   (org-agenda-files :maxlevel . 2) ; refile to any of these files
   ))

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
;; TOOD https://github.com/bmillwood/pointfree
(use-package haskell-mode
  :init (setq haskell-stylish-on-save t)
)
;; apply-refactor for hlint hints
(use-package hlint-refactor)
;; body insert
(fset 'haskellfuncbody
   [?0 ?v ?i ?w ?y ?o ?j ?f ?p backspace backspace backspace escape ?p ?a ?  ?= ?  escape])
(define-key haskell-mode-map (kbd "C-c f") 'haskellfuncbody)
;; ghcid from lukasz
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

;; calls pointfree (needs to be available on PATH) and replaces code with
;; its point free version
(defun dmw-haskell-pointfree-replace ()
  "Replaces the marked region with the Haskell pointfree evaluation."
  (interactive)
  (let ((pfcmd (format "pointfree %s"
                       (shell-quote-argument (buffer-substring-no-properties
                                              (region-beginning)
                                              (region-end))))))
    (shell-command-on-region (region-beginning) (region-end) pfcmd t)))

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
    ("y" yas-describe-tables "snippets")
    ("g" ghcid "ghcid")
    ("l" hlint-refactor-at-point "hlint (at point)")
    ("L" hlint-refactor-buffer "hlint (buffer)")
    )
   "Documentation"
   (("h" hoogle "hoogle"))))
;;(require 'flymake-hlint) ;; not needed if installed via package
;;(add-hook 'haskell-mode-hook 'flymake-hlint-load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Nix
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nix-mode
  :mode "\\.nix\\'")
(use-package nixpkgs-fmt)
;; format all nix files on save
(add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scala
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scala-mode)
(use-package sbt-mode)
(setq sbt:prefer-nested-projects t)    ;; bug in sbt-mode fixed

(setq compilation-auto-jump-to-first-error t)
(global-set-key (kbd "C-c s") 'sbt-hydra)

(add-hook 'sbt-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  `((,(expand-file-name (directory-file-name default-directory)) . ?⌂)
                    (,(expand-file-name "~") . ?~)))
            (prettify-symbols-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Smart programms running inside Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rest cliesnt is an amazing tool for calling http servers
(use-package restclient)
(use-package company-restclient)
(add-to-list 'company-backends 'company-restclient)
;; speed-type allows testing your type skills, just call speed-type-test
(use-package speed-type)

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
;; Smart Parens
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens)
(smartparens-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; direnv
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package direnv
 :config
 (direnv-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Statistics
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collects stats of key usage
(use-package keyfreq)
(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        previous-line
        next-line))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RSS reader
;; see: https://github.com/skeeto/elfeed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elfeed
  :config
  (setq elfeed-feeds
        '(
          ;; programming
          ("https://www.reddit.com/r/haskell.rss" haskell)
          ("https://www.reddit.com/r/emacs.rss" emacs)
  	)
  )
  (setq-default elfeed-search-filter "@2-days-ago +unread")
  (setq-default elfeed-search-title-max-width 100)
  (setq-default elfeed-search-title-min-width 100)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Needs cleanup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; quick way to update dependencies
(use-package auto-package-update)

;; auto-refresh all buffers when files have changed on disk
(global-auto-revert-mode t)
;; allows editing strings with escaping
(use-package string-edit)

;; highlight
(use-package highlight-symbol)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
;; remove whitespaces at the end of the line
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package multiple-cursors)
(global-set-key (kbd "C-s-c C-s-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

(use-package evil-mc)
(global-evil-mc-mode  1)

(use-package evil-surround)
(global-evil-surround-mode 1)
(use-package evil-nerd-commenter)


(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "."  'evilnc-copy-and-comment-operator
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
  "x" 'hydra-uiux/body
  "m" 'major-mode-hydra
)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(setq confirm-kill-emacs 'yes-or-no-p)

(load "~/.emacs.d/configs/install_first")
(load "~/.emacs.d/configs/yasnippet")
(load "~/.emacs.d/configs/greek")

;; The default value of custom-file is just the current user's .emacs.d/init.el file.
;; Emacs will add content to custom-file whenever a variable is customized or marked as safe.
;; When init.el is version controlled, it is quite annoying to have random machine-generated
;; variable settings added to it because those changes are often not worth keeping permanently,
;; so we set a different custom file here to avoid this situation.
;;
;; custom-before.el is loaded before the rest of init.el, while custom-after.el is loaded afterwards.
;; this-machine.el has customizations that should only apply to the current machine.
;; custom-before and custom-after are not version controlled in the dotfiles repo but they are shared across machines elsewhere.
(defvar machine-custom "~/.emacs.d/this-machine.el")
(defvar custom-after-file "~/.emacs.d/custom-after.el")
(setq custom-file "~/.emacs.d/custom-before.el")
(when (file-exists-p custom-file) (load custom-file))

;; (sxo-lib-say "Emacs loaded")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO - things that seem cool but I have not yet time to explore
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell:
;; - jump to definition in right window macro
;; https://www.gnu.org/software/emacs/manual/html_node/autotype/Hippie-Expand.html
;; https://github.com/hayamiz/twittering-mode/blob/3.0.x/README.markdown
;; https://github.com/fniessen/org-html-themes
;; https://pavpanchekha.com/blog/magit-unpublished.html
;; https://github.com/JulienMasson/org-gtasks
;; https://www.reddit.com/r/emacs/comments/8jaflq/tip_how_to_use_your_dashboard_properly/
;; https://github.com/emacs-evil/evil-collection
;; https://github.com/atykhonov/google-translate
;; https://github.com/Malabarba/emacs-google-this
;; https://github.com/dgutov/diff-hl
;; https://github.com/rejeep/f.el

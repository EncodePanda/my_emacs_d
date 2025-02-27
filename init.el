;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add package repositories.
;;
;; First we need to tell Emacs to use 'package' module. With it, we will be add
;; modules from the Internet.  The 'package-archives' holds the modules'
;; repositories
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
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
)


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
  :bind     ("C-M-SPC" . major-mode-hydra)
  :config   (evil-leader/set-key "m" 'major-mode-hydra))

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
;; UI/UX customizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode 0)             ;; no tool bar
(menu-bar-mode 0)             ;; no menu bar
(toggle-frame-fullscreen)     ;; start with fullscreen
(scroll-bar-mode 0)           ;; no scrollbar
(show-paren-mode 1)           ;; highlight matchin parenthesis
(column-number-mode 1)        ;; show column number in minibuffer
(global-linum-mode 1)         ;; line numbers
(fset `yes-or-no-p `y-or-n-p) ;; answer questions with y/n (instead of yes/no)

;; install icons
(use-package all-the-icons)
;; show icons when listing files and dirs in dired mode
(use-package all-the-icons-dired
  :hook     (dired-mode . all-the-icons-dired-mode))

;; Displays a vertical line showing the column length limit ( is set to 80)
(use-package display-fill-column-indicator
  :hook     (prog-mode . display-fill-column-indicator-mode)
            (org-mode . display-fill-column-indicator-mode)
            (latex-mode . display-fill-column-indicator-mode)
            (markdown-mode . display-fill-column-indicator-mode)
  :config   (setq-default fill-column 80))

;; The command 'clm/open-command-log-buffer' opens small buffer that shows all
;; the keystrokes and functions used while oparating Emacs
(use-package command-log-mode
  :config   (global-command-log-mode))

;; deeply nested delimiters (like parenthesis) get different colors, runs only
;; for programming modes
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook     (prog-mode . rainbow-delimiters-mode))

;; nyan cat flying in the minibuffer
(use-package nyan-mode
  :config   (nyan-mode 1))

;; words of encouragement in the minibuffer each time you save a file
(use-package encourage-mode
  :config   (encourage-mode 1)
            (setq encourage-encouragements (nconc encourage-encouragements
                                            '("Good job"
                                              "Great initiative"
                                              "Nice work"
					      "OMG! Awesome!"))))

;; load default theme called moe-theme in the dark version
(use-package moe-theme
  :config   (setq moe-dark-comment-delimiter -moe-dark-doc)
            (setq moe-dark-comment -moe-dark-doc)
            (load-theme 'moe-dark t))

;; allows quick theme change
(use-package helm-themes)

;; zoom in/out a.k.a presentation mode, this used to be part of melpa but not
;; anymore thus is now part of this repo
(load "~/.emacs.d/configs/frame-fns.el")
(load "~/.emacs.d/configs/frame-cmds.el")
(load "~/.emacs.d/configs/zoom-frm.el")

;; Show buffers that were opened recently. This is helpful if you just restarted
;; your Emacs
(use-package recentf
  :config   ;; store max 200 items
            (setq recentf-max-saved-items 200
             recentf-max-menu-items 15)
            ;; enable globallu
            (recentf-mode +1))

;; TODO make this a dedicated package that I will push to melpa
;; -----------------------------------------------------------------------------
;; The following allows you to switch the UI of cursor for both normal and
;; insert state depending if you switched to a light or dark theme.
;;
;; You have to define explicitly which themes are light themes and which are
;; dark themes by setting the light-themes-list and dark-themes-list variables


(defvar light-themes-list '(moe-light solarized-light leuven)
  "List of light themes.")

(defvar dark-themes-list '(moe-dark)
  "List of dark themes.")

(defun cursors-to-light ()
  "Modify cursor types and colors for different evil modes."
  (interactive)
  (setq evil-insert-state-cursor '(bar "black"))
  (setq evil-normal-state-cursor '(box "black")))

(defun cursors-to-dark ()
  "Modify cursor types and colors for different evil modes."
  (interactive)
  (setq evil-insert-state-cursor '(bar "white"))
  (setq evil-normal-state-cursor '(box "yellow")))

(defun light-theme-advice (&rest args)
  "Advice for `load-theme' to activate `cursors-to-light' when a light theme is loaded."
  (when (member (car args) light-themes-list)
    (cursors-to-light)
    (message "%s theme loaded, cursors-to-light called" (car args))))

(defun dark-theme-advice (&rest args)
  "Advice for `load-theme' to activate `cursors-to-dark' when a dark theme is loaded."
  (when (member (car args) dark-themes-list)
    (cursors-to-dark)
    (message "%s theme loaded, cursors-to-dark called" (car args))))

(advice-add 'load-theme :after #'light-theme-advice)
(advice-add 'load-theme :after #'dark-theme-advice)
;; -----------------------------------------------------------------------------

;; This Hydra creates a menu to manipulate the UX/UI elements quickly
(pretty-hydra-define hydra-uiux (:foreign-keys warn
				 :title "UI/UX"
				 :quit-key "q")
  ("Zoom"
   (("i" zoom-frm-in "(+) in")
    ("o" zoom-frm-out "(-) out")
   )
   "Volume"
   (("m" osx-lib-mute-volume "mute")
    ("M" osx-lib-unmute-volume "unmute")
   )

   "Other"
   (("t" helm-themes "theme switch")
    ("l" clm/open-command-log-buffer "log commands buffer")
   )
   ))

;; enable UIUI hydra under SPC-x
(evil-leader/set-key
  "x" 'hydra-uiux/body
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm - incremental completion and selection narrowing framework
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm
  :config   ;; replace default M-x
            (global-set-key (kbd "M-x") 'helm-M-x)
            ;; replace default find file
            (global-set-key (kbd "C-x C-f") 'helm-find-files)
            ;; replace default search
            (global-set-key (kbd "C-s") 'helm-occur))


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
  "pG" 'helm-do-ag                        ;; grap content in the project
                                          ;; but you specify the folder
  "ps" 'projectile-save-project-buffers   ;; save all buffers in the project
  "ph" 'helm-projectile-switch-project    ;; switch to known project
  "pc" 'project-compile                   ;; compile current project
)

;; Dired tries to guess a default target directory. This means if there is a
;; Dired buffer displayed in some window, use its current directory, instead of
;; this Dired buffer's current directory.
(setq dired-dwim-target t)

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
(use-package evil-iedit-state
  :after    (iedit evil evil-collection)
  :config   (define-key evil-normal-state-map (kbd "gc") 'evil-iedit-state/iedit-mode))

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

(use-package format-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialize Dashboard
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dashboard
  :config (dashboard-setup-startup-hook))
          (setq dashboard-items '((recents  . 5)
          		       (bookmarks . 5)
          		       (projects . 5)
          		       (agenda . 5)
          		       (registers . 5)))
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

;; window management made easy
(use-package ace-window
  ;; changing windows with row-key
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
)

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

;; mark commits already seen on the log list
(use-package magit-commit-mark
  :commands (magit-commit-mark-mode))

(eval-after-load 'magit
  (add-hook 'magit-mode-hook 'magit-commit-mark-mode))

;; lists items in a file todo.org
;; good for temp list of TODOS while working on a branch
(use-package magit-org-todos
  :config
  (magit-org-todos-autoinsert))

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
(setq company-idle-delay 1
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
  :bind     (("C-c l" . 'org-store-link)
             ("C-c a"  . 'org-agenda)
             ("C-c c"  . 'org-capture))

  :hook     ('org-mode-hook . org-autolist-mode)
  :init

  ;; add timestamp when closing as done
  (setq org-log-done 'time)
  ;; add closing note
  (setq org-log-done 'note)

  ;; export to HTML converts straight quotes to smart quotes ("curly quotes")
  ;; and converting hyphens --- to —
  (setq org-export-with-smart-quotes t)

  ;; adds CREATED property to newly created heading
  (setq org-log-into-drawer t)

  (setq org-adapt-indentation t
        org-hide-leading-stars t
        org-src-fontify-natively t
        org-edit-src-content-indentation 0)

  (setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "TO_READ(r)" "READING(g)" "|" "IS_READ(i)")
        (sequence "|" "CANCELED(c)")))

  ;; do not include validate link when exporting to html
  (setq org-html-validation-link nil)

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  (progn
    (setq org-agenda-files (directory-files-recursively "/Users/encodepanda/Dropbox/org" "\\.org$"))
    (setq org-default-notes-file (concat org-directory "/captured.org"))
    (add-hook 'org-mode-hook 'org-toggle-pretty-entities) ;; pretty math latex
  )
)

(defun insert-org-link-from-url ()
  "Fetches the title of a webpage from a URL in the clipboard and inserts an org-mode link at point."
  (interactive)
  (let* ((url (substring-no-properties (current-kill 0)))
         (buffer (url-retrieve-synchronously url))
         title)
    (with-current-buffer buffer
      (goto-char (point-min))
      (if (re-search-forward "<title>\\(.*?\\)</title>" nil t)
          (setq title (match-string 1))
        (setq title url)) ; Use URL as title if no title is found
      (kill-buffer buffer))
    (insert (format "[[%s][%s]]" url title))))

(use-package org-superstar
  :after    org
  :config   (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
           ;;; Titles and Sections
           ;; hide #+TITLE:
           (setq org-hidden-keywords '(title))
           ;; set basic title font
           (set-face-attribute 'org-level-8 nil :weight 'bold :inherit 'default)
           ;; Low levels are unimportant => no scaling
           (set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
           (set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
           (set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
           (set-face-attribute 'org-level-4 nil :inherit 'org-level-8)
           ;; Top ones get scaled the same as in LaTeX (\large, \Large, \LARGE)
           (set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.0) ;\large
           (set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.2) ;\Large
           (set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.44) ;\LARGE
           ;; Only use the first 4 styles and do not cycle.
           (setq org-cycle-level-faces nil)
           (setq org-n-level-faces 4)
           ;; Document Title, (\huge)
           (set-face-attribute 'org-document-title nil
                               :height 2.074
                               :foreground 'unspecified
                               :inherit 'org-level-8)

  )

(use-package org-pomodoro
  :after    org)

;; TODO use https://www.nongnu.org/org-edna-el/

;; Template expansion for Org structures
;; User <s TAB to insert a source block
(use-package org-tempo
  :after    org)

(setq org-agenda-files-org (directory-files-recursively "/Users/encodepanda/Dropbox/org" "\\.org$"))
(setq org-agenda-files-work (directory-files-recursively "/Users/encodepanda/Dropbox/org-work" "\\.org$"))

(defun switch-org-agenda-files-set (set-name)
  (cond ((equal set-name "org") (setq org-agenda-files org-agenda-files-org))
        ((equal set-name "work") (setq org-agenda-files org-agenda-files-work))
        (t (error "Invalid set name"))))

(defvar helm-source-org-agenda-files-sets
  (helm-build-sync-source "Org Agenda File Sets"
    :candidates '("org" "opensource" "work")
    :action '(("Switch to set" . switch-org-agenda-files-set))))

(defun helm-org-agenda-files-sets ()
  (interactive)
  (helm :sources 'helm-source-org-agenda-files-sets
        :buffer "*helm org agenda sets*"))


(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
    )

(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
        (dot . t)
        (latex . t)
    )
)
;; TODO https://orgmode.org/worg/exporters/taskjuggler/ox-taskjuggler.html
(use-package org-contrib)
(use-package ox-reveal
  :after org
  :init
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
  (setq org-reveal-hlevel 2)
  (setq org-reveal-control nil)
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
   ("j" "JIRA" entry (file+headline "~/org/work/tripshot/issues.org" "JIRA issues")
        "* TODO %?\n  %i\n  %a")
   ("w" "Work log" entry (file+datetree "~/org/work/tripshot/diary.org")
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

(fset 'haskell-function-body
   (kmacro-lambda-form [?I escape ?v ?i ?w ?y ?o backspace backspace escape ?p ?a ?  ?= ?  ?q backspace escape] 0 "%d"))
(define-key haskell-mode-map (kbd "C-c f") 'haskell-function-body)

;; apply-refactor for hlint hints
(use-package hlint-refactor)
;; ghcid from lukasz
(load "~/.emacs.d/configs/ghcid.el")
(use-package haskell-snippets)
(require 'haskell-snippets)

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
(setq temporary-file-directory "~/.emacs.d/tmp/")

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
(sp-local-pair 'org-mode "~" "~")

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
;; Tools and programs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world clock
(setq display-time-world-list
      '(("Europe/London"                    "London - UK")
        ("Europe/Warsaw"                    "Warsaw - PL")
        ("Australia/Canberra"               "Canberra - AU")))

(use-package helpful)

;; TODO create hydra for helpful


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO - things that seem cool but I have not yet time to explore
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell:
;; - jump to definition in right window macro
;; https://www.gnu.org/software/emacs/manual/html_node/autotype/Hippie-Expand.html
;; https://github.com/hayamiz/twittering-mode/blob/3.0.x/README.markdown
;;
;; https://github.com/atykhonov/google-translate
;; https://github.com/Malabarba/emacs-google-this
;; https://github.com/dgutov/diff-hl
;;

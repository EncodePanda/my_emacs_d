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
(tool-bar-mode 0)                    ;; no tool bar
(menu-bar-mode 0)                    ;; no menu bar
(toggle-frame-fullscreen)            ;; start with fullscreen
(scroll-bar-mode 0)                  ;; no scrollbar
(show-paren-mode 1)                  ;; highlight matchin parenthesis
(column-number-mode 1)               ;; show column number in minibuffer
(global-display-line-numbers-mode 1) ;; display line numbers
(fset `yes-or-no-p `y-or-n-p)        ;; answer questions with y/n (instead of
;; yes/no)

;; sometimes we want to dedicate a window for a given buffer, this function
;; allows to quickly toggle between dedicated and not dedicated for a given
;; buffer
(defun toggle-window-dedicated ()
  "Toggle whether the current window is dedicated to its buffer."
  (interactive)
  (let* ((window (selected-window))
         (state (window-dedicated-p window)))
    (set-window-dedicated-p window (not state))
    (message "Window %s" (if (not state) "dedicated" "undedicated"))))

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

;; allows quick theme change (helm-themes was removed from melpa,
;; so we use helm directly to pick themes)
(defun helm-themes--load (theme)
  "Disable current themes then load THEME."
  (mapc 'disable-theme custom-enabled-themes)
  (unless (string= theme "default")
    (load-theme (intern theme) t)))

(defun helm-themes--candidates ()
  "Return list of available themes with `default' on the head."
  (cons 'default (custom-available-themes)))

(defun helm-choose-theme ()
  "Choose a theme interactively using helm."
  (interactive)
  (let (helm-candidate-number-limit
        (orig-theme (or (car-safe custom-enabled-themes) 'default)))
    (unless (helm :prompt (format "pattern (current theme: %s): " orig-theme)
                  :preselect (format "%s$" orig-theme)
                  :sources (helm-build-sync-source "Theme"
                             :candidates 'helm-themes--candidates
                             :action 'helm-themes--load
                             :persistent-action 'helm-themes--load)
                  :buffer "*helm-themes*")
      (helm-themes--load (symbol-name orig-theme)))))

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

(defvar dark-themes-list '(moe-dark manoj-dark tango-dark leuven-dark afternoon-theme)
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
   "Window"
   (("e" enlarge-window "enlarge")
    ("s" shrink-window "enlarge")
    )

   "Other"
   (("t" helm-choose-theme "theme switch")
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
(use-package projectile
  :config ;; follow the compilation buffer
  (setq compilation-scroll-output t)
  )
(projectile-global-mode 1)
(use-package helm-projectile) ;; integrate helm with projectile
(use-package ag)              ;; search using ag (silversearch)
(use-package helm-ag)         ;; helm integration with ag


(defun my-compilation-buffer-colorize ()
  "Apply ANSI color codes in compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook #'my-compilation-buffer-colorize)


(defun helm-projectile-git-modified-files ()
  "Helm over Git modified (staged + unstaged) files in current Projectile project."
  (interactive)
  (let* ((project-root (projectile-project-root)))
    (unless project-root
      (error "Not inside a Projectile project"))
    (let* ((default-directory project-root)
           (git-cmd "git diff --name-only HEAD")
           (modified-files (split-string (shell-command-to-string git-cmd) "\n" t))
           (helm-source (helm-build-sync-source "Git Modified Files"
                          :candidates modified-files
                          :fuzzy-match t
                          :action '(("Open file" . find-file)
                                    ("Open in other window" . find-file-other-window)))))
      (helm :sources helm-source
            :buffer "*helm git modified files*"))))

(defun open-new-project ()
  "Open a new projectile project by specifying name and root directory."
  (interactive)
  (let* ((project-name (read-string "Project name: "))
         (root-dir (read-directory-name "Root directory: " "~/projects/"))
         (project-path (expand-file-name project-name root-dir)))
    (if (file-directory-p project-path)
        (let ((default-directory project-path))
          (projectile-add-known-project project-path)
          (helm-projectile-find-file))
      (message "Directory does not exist: %s" project-path))))

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
  "pn" 'open-new-project                  ;; open new project by name
  "pc" 'projectile-compile-project        ;; compile current project
  "pt" 'projectile-test-project           ;; test current project
  "pe" 'projectile-run-project            ;; execute (run) current project
  "pl" 'project-shell                     ;; project shell
  )

(put 'project-compile-command 'safe-local-variable #'stringp)

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
  "nk" 'flycheck-previous-error
  "nj" 'flycheck-next-error
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

;; type gs to add ; at the end of the line
(evil-define-key 'normal 'global
  (kbd "gs")
  (lambda ()
    (interactive)
    (end-of-line)
    (insert ";")))


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
;; TODO consider alternative with saved buffers
;; https://github.com/nex3/perspective-el?tab=readme-ov-file#installation
;; like linux multiple desktop support
(use-package eyebrowse
  :config (eyebrowse-mode t)
  )

(use-package eyebrowse-restore
  :ensure t
  :config
  (eyebrowse-restore-mode))

;; Define a Helm source for Eyebrowse
(defun helm-eyebrowse-source ()
  "Create a Helm source for Eyebrowse window configurations."
  (helm-build-sync-source "Eyebrowse Configurations"
    :candidates (mapcar (lambda (config)
                          (let ((index (car config))
                                (name (eyebrowse-format-slot config)))
                            (cons (format "[%d] %s" index name) index)))
                        (eyebrowse--get 'window-configs))
    :action (lambda (index)
              (eyebrowse-switch-to-window-config index))))

;; Create a Helm command to list and select Eyebrowse configurations
(defun helm-eyebrowse ()
  "List and select Eyebrowse window configurations using Helm."
  (interactive)
  (helm :sources (helm-eyebrowse-source)
        :buffer "*helm-eyebrowse*"))

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
  "we" 'helm-eyebrowse
  "w1" 'eyebrowse-switch-to-window-config-1
  "w2" 'eyebrowse-switch-to-window-config-2
  "w3" 'eyebrowse-switch-to-window-config-3
  "w4" 'eyebrowse-switch-to-window-config-4
  "w5" 'eyebrowse-switch-to-window-config-5
  "w6" 'eyebrowse-switch-to-window-config-6
  "w5" 'eyebrowse-switch-to-window-config-7
  "w8" 'eyebrowse-switch-to-window-config-8
  "w9" 'eyebrowse-switch-to-window-config-9
  "wtp" 'tab-line-switch-to-prev-tab
  "wtn" 'tab-line-switch-to-next-tab
  "wv" 'split-window-horizontally
  "wh" 'split-window-vertically
  "wb" 'balance-windows
  "wx" 'ace-delete-window
  "wk" 'delete-other-windows         ;; deletes other windows, no questions asked
  "wu" 'winner-undo                  ;; undo any window change
  "wU" 'winner-redo                  ;; redo any window change
  "k" 'kill-buffer
  )

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
  :after magit
  :hook (magit-mode . magit-commit-mark-mode)
  :commands (magit-commit-mark-mode)
  )


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
;;(use-package git-gutter-fringe+)
;;(global-git-gutter+-mode t)
;; refresh gutter when staged by magit
;;(defun my-refresh-visible-git-gutter-buffers ()
;;  (dolist (buff (buffer-list))
;;    (with-current-buffer buff
;;      (when (and git-gutter+-mode (get-buffer-window buff))
;;        (git-gutter+-mode t)))))

;;(add-hook 'magit-post-refresh-hook
;;          #'my-refresh-visible-git-gutter-buffers)

;; inline git blame
;;(use-package git-messenger)
;;(setq git-messenger:show-detail t) ;; more details in popup (author, date etc.)

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
(use-package company
  :init
  ;; (setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-dabbrev company-ispell)))
  :config (global-company-mode)

  ;; (add-to-list 'company-backends 'company-yasnippet)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; Use M-RET to accept completion and trigger LSP additional text edits (like auto-imports)
  (define-key company-active-map (kbd "C-RET") 'company-complete-selection)
  (define-key company-active-map (kbd "C-<return>") 'company-complete-selection)
  :bind ("C-<tab>" . 'company-complete-common-or-cycle)
  )
;; "aggressive" completion (no delays, quick feedback)
(setq company-idle-delay 1
      company-echo-delay 0
      company-dabbrev-downcase nil
      company-minimum-prefix-length 3
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))

(use-package company-box
  :hook (company-mode . company-box-mode))

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
  ;; Set this to wherever your org files live:
  (setq org-directory (expand-file-name "~/org"))
  ;; Now it’s safe to derive paths from org-directory
  (setq org-default-notes-file (expand-file-name "captured.org" org-directory))


  ;; add timestamp when closing as done
  (setq org-log-done 'time)
  ;; add closing note
  (setq org-log-done 'note)

  (setq org-export-exclude-tags '("noexport"))

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

  )


(evil-leader/set-key
  "olf" 'org-web-tools-insert-link-for-url
  "ocr" 'org-refile
  "occ" 'helm-org-capture-templates
  "oa" 'org-agenda
  "ow" 'helm-org-agenda-files-sets ;; set which org: private or work
  )

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
;; TODO build org-mode hydra

;; Define base directories and file extension pattern
(defcustom org-agenda-base-dir "/Users/encodepanda/Dropbox/org"
  "Base directory for general org files.")
(defcustom work-agenda-base-dir "/Users/encodepanda/Dropbox/org-work"
  "Base directory for work org files.")
(defcustom org-agenda-file-pattern "\\.org$"
  "File extension pattern for org files.")

;; Generate file lists
(setq org-agenda-files-org (directory-files-recursively org-agenda-base-dir org-agenda-file-pattern))
(setq org-agenda-files-work (directory-files-recursively work-agenda-base-dir org-agenda-file-pattern))

;; Define capture templates
(setq org-capture-templates-private
      '(("t" "Todo" entry (file+headline "~/org/capture.org" "Tasks")
         "* TODO %?\n  %i")
        ("d" "Diary" entry (file+datetree "~/org/diary.org")
         "* %? %U")))

(setq org-capture-templates-work
      '(("t" "Todo" entry (file+headline "~/org-work/capture.org" "Capture")
         "* TODO %?\n  %i")
        ("p" "Plan" entry (file+datetree "~/org-work/plan.org")
         "* TODO %?\n  %i\n  SCHEDULED: %t")
        ("w" "Work log" entry (file+datetree "~/org-work/work-log.org")
         "* %? %U")
        ("m" "Meeting" entry (file+datetree "~/org-work/meetings.org")
         "* %? %U")
	))

;; Function to switch org-agenda-files and capture templates set
(defun switch-org-agenda-files-set (set-name)
  (interactive)
  (cond ((equal set-name "org")
         (setq org-agenda-files org-agenda-files-org)
         (setq org-capture-templates org-capture-templates-private))
        ((equal set-name "work")
         (setq org-agenda-files org-agenda-files-work)
         (setq org-capture-templates org-capture-templates-work))
        (t (error "Invalid set name: %s" set-name))))

;; Helm source for selecting agenda file sets
(defvar helm-source-org-agenda-files-sets
  (helm-build-sync-source "Org Agenda File Sets"
    :candidates '("org" "work")
    :action '(("Switch to set" . switch-org-agenda-files-set))))

;; Helm function for switching org-agenda-files sets
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


(setq org-agenda-files org-agenda-files-org)
(setq org-capture-templates org-capture-templates-private)


(defun markdown-convert-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

;; add to eneeded to produce latex/pdf for org more
(if (eq window-system 'mac)
    (add-to-list 'exec-path "/usr/local/texlive/2019/bin/x86_64-darwin")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rust
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rust-cargo-add-crate ()
  "Prompt for a crate name and run cargo add to add it to the project."
  (interactive)
  (let ((crate-name (read-string "Crate name: ")))
    (when (and crate-name (not (string-empty-p crate-name)))
      (let ((default-directory (projectile-project-root)))
        (compile (concat "cargo add " crate-name))))))

(major-mode-hydra-define rust-mode nil
  ("Navigation"
   (("f" rust-beginning-of-defun  "function beginning")
    ("F" rust-end-of-defun  "function end")
    )
   "LSP"
   (("e" lsp-rust-analyzer-expand-macro  "expand macro")
    ("c" lsp-rust-analyzer-open-cargo-toml  "open Cargo.toml")
    )
   "Cargo"
   (("a" rust-cargo-add-crate  "add crate")
    )
   ))

(setq-default format-all-formatters '(("Rust" rustfmt)))


;; TOOD https://github.com/bmillwood/pointfree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Haskell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOOD https://github.com/bmillwood/pointfree
(use-package haskell-mode
  :init (setq haskell-stylish-on-save t)
  )

;; from function definition to body
(fset 'haskell-function-body
      (kmacro-lambda-form [?I escape ?v ?i ?w ?y ?o backspace backspace escape ?p ?a ?  ?= ?  ?q backspace escape] 0 "%d"))
(define-key haskell-mode-map (kbd "C-c f") 'haskell-function-body)

;; apply-refactor for hlint hints
(use-package hlint-refactor)
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
   (("o" haskell-navigate-imports  "imports")
    ("c" haskell-cabal-visit-file "cabal visit")
    ("h" haskell-hoogle "hoogle")
    )
   "Editing"
   (("i" haskell-imports-helm  "imports")
    ("y" yas-describe-tables "snippets")
    ("l" flymake-show-buffer-diagnostics "hlint (buffer)")
    ("b" haskell-function-body "insert function body")
    )
   "Documentation"
   (("h" hoogle "hoogle"))))
(use-package flymake-hlint)
(add-hook 'haskell-mode-hook 'flymake-hlint-load)
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
;; (add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode)

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
;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun toggle-delete-trailing-whitespace ()
  (interactive)
  (if (member #'delete-trailing-whitespace before-save-hook)
      (progn
        (remove-hook 'before-save-hook #'delete-trailing-whitespace)
        (message "delete-trailing-whitespace disabled"))
    (add-hook 'before-save-hook #'delete-trailing-whitespace)
    (message "delete-trailing-whitespace enabled")))



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LSP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If you want LSP to load automatically on start-up for any Haskell project,
;; add following hook
;;
;; Some configurations are based on
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;
;; TODO  https://www.reddit.com/r/emacs/comments/ezxtc5/lspmode_show_errors/
;;
;; Turn on LSP for a project: M-x lsp
;; Turn off LSP for a project: M-x lsp-disconnect
;; -----------------------------------------------------------------------------
(use-package lsp-mode      :commands    lsp
  :hook        (lsp-mode . lsp-enable-which-key-integration)
  :hook        (haskell-mode . lsp)
  :hook        (rust-mode . lsp-deferred)

  :custom      (lsp-prefer-flymake nil)
  :config      (push "[/\\\\]\\.stack-work\\'" lsp-file-watch-ignored-directories)
  (push "[/\\\\]\\.dist-newstyle\\'" lsp-file-watch-ignored-directories)
  (push "[/\\\\]\\.dist-newstyle\\'" lsp-file-watch-ignored-directories)
  (advice-add 'lsp :before #'direnv-update-environment)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  ;; Enable auto-import on completion
  (setq lsp-completion-enable-additional-text-edit t)
  )
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; -----------------------------------------------------------------------------
(use-package lsp-haskell)
;; -----------------------------------------------------------------------------
;; Rust-analyzer specific configuration
(with-eval-after-load 'lsp-mode
  (setq lsp-rust-analyzer-completion-add-call-parenthesis t)
  ;; rust-analyzer expects cargo.extraEnv to be a JSON object/map
  (setq lsp-rust-analyzer-cargo-extra-env (make-hash-table :test 'equal))
  (setq lsp-rust-analyzer-completion-add-call-argument-snippets t))

;; -----------------------------------------------------------------------------
(use-package lsp-ui        :hook        (lsp-mode . lsp-ui-mode)
  :custom      (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'at-point))
;; -----------------------------------------------------------------------------

(with-eval-after-load 'lsp-ui-doc

  (defun my/lsp-ui-doc-unfocus-and-hide ()
    "Unfocus lsp-ui-doc child frame and hide it."
    (interactive)
    (when (fboundp 'lsp-ui-doc-unfocus-frame)
      (lsp-ui-doc-unfocus-frame))
    (cond
     ((fboundp 'lsp-ui-doc-hide) (lsp-ui-doc-hide))
     (t                          (lsp-ui-doc-toggle))))
  )

(evil-leader/set-key
  "lr"  'lsp-rename
  "la"  'helm-lsp-code-actions
  "ll"  'lsp-avy-lens
  "ldg" 'lsp-ui-doc-glance
  "ldf" 'lsp-ui-doc-focus-frame
  "ldu" 'lsp-ui-doc-unfocus-and-hide
  "lfr" 'lsp-ui-peek-find-references
  "lfi" 'lsp-ui-peek-find-implementation
  "lfd" 'lsp-find-definition
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tools and programs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world clock
(setq display-time-world-list
      '(("Europe/London"                    "London - UK")
        ("Europe/Warsaw"                    "Warsaw - PL")
        ("America/Los_Angeles"              "Los Angeles")
	("America/Denver"                   "Denver")
        ("Australia/Canberra"               "Canberra - AU")))

(use-package helpful)

;; TODO create hydra for helpful



(use-package pulsar
  :hook     ('next-error-hook . 'pulsar-pulse-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AI, Agents and LLMs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO https://github.com/douo/magit-gptcommit
;; seems very powerful https://github.com/lanceberge/elysium

;; Copilot
(use-package copilot)

(add-hook 'rust-mode-hook 'copilot-mode)
(add-hook 'haskell-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-l") 'copilot-accept-completion-by-line)


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
;; https://www.reddit.com/r/emacs/comments/18da57j/evil_mode_how_to_move_lines_up_and_down_in_visual/
;; drag-stuff
;; https://prelude.emacsredux.com/en/latest/ - steal packages they use :)
;;
;; https://github.com/akicho8/string-inflection - jump between cases
;; https://ryan.himmelwright.net/post/org-babel-setup/ org-mode configuration
;; https://github.com/gopar/.emacs.d cool inspiration
;;
;; Emcas as Postgres IDE:
;;
;; * https://emacs-lsp.github.io/lsp-mode/page/lsp-sqls/ and
;; * https://emacs-lsp.github.io/lsp-mode/page/lsp-sqls/
;;
;; Another amazing source of inspiration https://github.com/MatthewZMD/.emacs.d
;;
;; AI agents
;; * https://www.tabnine.com/

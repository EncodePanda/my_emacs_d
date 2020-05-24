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
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  )

;; uses oxs notifier as default TODO not sure if its working ...
(setq alert-default-style 'osx-notifier)

;; osx-lib gives nice adapters to OSX functionality
(use-package osx-lib)
(osx-lib-say "Emacs loading...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Evil-mode - provides Vim features like Visual selection and text objects
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key-chord allows usage of key-chord-define-global
(use-package key-chord)
(key-chord-mode 1)
;; enable evil mode
(use-package evil
  :config
  (evil-mode)
)
;; when in normal mode, cursor will be heyllow
(setq evil-normal-state-cursor '(box "yellow"))
(setq evil-insert-state-cursor '(bar "white"))
(key-chord-define-global "fj" 'evil-normal-state)
;; leader key provides the <leader> feature from Vim that provides an easy way
;; to bind keys under a variable prefix key
(use-package evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

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
  :bind
  ("C-M-SPC" . major-mode-hydra))

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

;; adds relative line numbers in normal mode and absolute
;; line numbers in insert mode
;; to use *just* absolute line numbers everywhere we would have to
;; replace code below with (global-linum-mode 1)
(use-package nlinum-relative
    :config
    (nlinum-relative-setup-evil)
    (add-hook 'prog-mode-hook 'nlinum-relative-mode))

;; answer questions with y/n (instead of yes/no)
(fset `yes-or-no-p `y-or-n-p)

;; load theme
(use-package moe-theme)
(load-theme 'moe-dark t)

;; quick-switch-themes allows to quickly toggle between defined themes
(defvar quick-switch-themes
  (let ((themes-list (list 'moe-dark
                           'moe-light
			   'light-blue)))
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

;; nyancut flying around :)
(use-package nyan-mode)
(nyan-mode 1)

;; encourage your work each time you save a buffer :)
(use-package encourage-mode)
(encourage-mode 1)

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
   (("t" quick-switch-themes* "theme switch")
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
  "pr" 'helm-projectile-recentf           ;; project recently opened buffers
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
  "d" 'dumb-jump-go-prompt
  )
;; note that by default gd is bound to evil-definition that is using tags
;; with dumb jump we can completely ignore etags
(define-key evil-normal-state-map (kbd "gd") 'dumb-jump-go)
(define-key evil-normal-state-map (kbd "gD") 'dumb-jump-back)
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

(global-set-key (kbd "M-]") 'next-buffer)     ;; to buffer on the left
(global-set-key (kbd "M-[") 'previous-buffer) ;; to buffer on the right
(evil-leader/set-key
  "ww" 'ace-window
  "ws" 'ace-swap-window
  "w1" 'eyebrowse-switch-to-window-config-1
  "w2" 'eyebrowse-switch-to-window-config-2
  "w3" 'eyebrowse-switch-to-window-config-3
  "w4" 'eyebrowse-switch-to-window-config-4
  "wv" 'split-window-horizontally
  "wh" 'split-window-vertically
  "wx" 'ace-delete-window
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
(use-package evil-magit) ;; integration with evil

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
(use-package company
  :config
  (add-to-list 'company-backends 'company-etags)
;; (add-to-list 'company-backends 'company-cabal)
;; (add-to-list 'company-backends 'company-dabbrev-code)
;; (add-to-list 'company-backends 'company-yasnippet)
;; (add-to-list 'company-backends 'company-files)
)
(global-company-mode t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spell-checkng
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell)
(use-package flyspell-correct-helm
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-helm))

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
(use-package haskell-mode
  :init (setq haskell-stylish-on-save t)
)
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
    )
   "Documentation"
   (("h" hoogle "hoogle"))))
;;(require 'flymake-hlint) ;; not needed if installed via package
;;(add-hook 'haskell-mode-hook 'flymake-hlint-load)

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

;; etags
(use-package etags-select)
(use-package helm-etags-plus)

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
  "g" 'hydra-git/body
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
(load "~/.emacs.d/configs/misc")
(load "~/.emacs.d/configs/org")
(load "~/.emacs.d/configs/greek")
(osx-lib-say "Emacs loaded")

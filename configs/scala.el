(packages-conditional-install '(ensime scala-mode yasnippet))

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(require 'yasnippet)


(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/my_snippets"
	))
(yas-reload-all)
(add-hook 'scala-mode-hook #'yas-minor-mode)
(with-eval-after-load 'company
  (define-key company-active-map [tab] nil)
  )

(add-hook 'scala-mode-hook #'linum-mode)

;; ignore first test
(defun ignore-fst-test ()
  "Change me!"
  (interactive)
  (beginning-of-buffer nil)
  (isearch-forward nil 1)
  (isearch-printing-char 116 1)
  (isearch-printing-char 101 1)
  (isearch-printing-char 115 1)
  (isearch-printing-char 116 1)
  (isearch-printing-char 40 1)
  (isearch-printing-char 34 1)
  (isearch-exit)
  (forward-word 1)
  (backward-word 1)
  (backward-word 1)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (insert "ignore"))

(defun unignore-fst-test ()
  "Change me!"
  (interactive)
  (beginning-of-buffer nil)
  (isearch-forward nil 1)
  (isearch-printing-char 105 1)
  (isearch-printing-char 103 1)
  (isearch-printing-char 110 1)
  (isearch-printing-char 111 1)
  (isearch-printing-char 114 1)
  (isearch-printing-char 101 1)
  (isearch-printing-char 40 1)
  (isearch-printing-char 34 1)
  (isearch-exit)
  (backward-word 1)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (insert "test"))



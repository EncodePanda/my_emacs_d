;; all yasnippet configs + functions used
(packages-conditional-install '(yasnippet))
(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/my_snippets"
	))

(yas-global-mode 1)

(defun find-git-repo (dir)
  (if (string= "/" dir)
      nil
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))



(defun find-project-root ()
  (interactive)
  (if (ignore-errors (eproject-root))
      (eproject-root)
    (or (find-git-repo (buffer-file-name)) (file-name-directory (buffer-file-name)))))

(defun file-path-to-namespace ()
  (interactive)
  (let (
        (root (find-project-root))
        (base (file-name-nondirectory buffer-file-name))
        )
    (substring (replace-regexp-in-string "/" "\\" (substring buffer-file-name (length root) (* -1 (length base))) t t) 0 -1)
    )
  )

(defun find-scala-dir (dir)
  (if (string= "/" dir)
      nil
    (if (file-exists-p (expand-file-name "scala/" dir))
        dir
      (find-scala-dir (expand-file-name "../" dir)))))

(defun find-scala-root ()
  (interactive)
  (if (ignore-errors (eproject-root))
      (eproject-root)
    (or (find-scala-dir (buffer-file-name)) (file-name-directory (buffer-file-name)))))


(defun file-path-to-package ()
  (interactive)
  (let (
        (root (find-scala-root))
        (base (file-name-nondirectory buffer-file-name))
        )
    (substring (replace-regexp-in-string "/" "." (substring buffer-file-name (+ (length root) 6) (* -1 (length base))) t t) 0 -1)
    )
  )

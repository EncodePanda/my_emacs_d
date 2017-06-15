(packages-conditional-install '(goto-chg multiple-cursors avy undo-tree string-edit highlight-symbol))

(key-chord-define-global "kw" 'kill-word)
(key-chord-define-global "bw" 'backward-kill-word)
(key-chord-define-global "mw" 'mark-word)
(key-chord-define-global "ms" 'mark-sexp)

(require 'eno)
(key-chord-define-global "wj" 'eno-word-goto)

(key-chord-define-global "cj" 'avy-goto-subword-1)
(global-set-key (kbd "C-c j") 'avy-goto-subword-1)

;; highlight
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(global-set-key
 (kbd "C-h")
 (defhydra hydra-highlight ()
   "highlight"
   ("n" highlight-symbol-next "next")
   ("p" highlight-symbol-prev "previous")
   ("q" nil "quit")

   ))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; mini-vi
(defhydra hydra-vi (:pre (set-cursor-color "#40e0d0")
                    :post (progn
                            (set-cursor-color "#ffffff")
                            (message
                             "Thank you, come again.")))
  "vi"
  ("l" forward-char "forward")
  ("h" backward-char "backward")
  ("j" next-line "next")
  ("k" previous-line "previous")
  ("o" sp-backward-sexp "back_paren")
  ("p" sp-forward-sexp "forward_paren")
  ("q" nil "quit"))


;; zoom
(defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

(global-set-key (kbd "C-c g") 'goto-line)

(defun uncomment-block ()
  "uncomments the /* block */"
  (interactive)
  (isearch-forward nil 1)
  (isearch-printing-char 42 1)
  (isearch-printing-char 47 1)
  (isearch-exit)
  (delete-backward-char 1 nil)
  (delete-backward-char 1 nil)
  (isearch-backward nil 1)
  (isearch-printing-char 47 1)
  (isearch-printing-char 42 1)
  (isearch-exit)
  (delete-forward-char 1 nil)
  (delete-forward-char 1 nil)
  (kmacro-end-or-call-macro nil))



(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
  (next-line))
(global-set-key (kbd "C-c /") 'toggle-comment-on-line)
(key-chord-define-global "cl" 'toggle-comment-on-line)


(defun copy-line (arg)
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))


(defun double-line (arg)
  "copy line and place it below the original"
  (interactive "p")
  (copy-line arg)
  (yank)
  (move-end-of-line))
(global-set-key (kbd "C-c d") 'double-line)


(global-set-key [(control .)] 'goto-last-change)
(global-set-key (kbd "C-s-c C-s-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(global-undo-tree-mode 1)

;; undo/redo
(defhydra hydra-undo-tree (:color yellow
                                  :hint nil
                                  )
  "
  _p_: undo  _n_: redo _s_: save _l_: load   "
  ("p"   undo-tree-undo)
  ("n"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("v"   undo-tree-visualize "visualize" :color blue)
  ("c"   nil "quit" :color blue))

(global-set-key (kbd "C-c u") 'hydra-undo-tree/body)


(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
  (next-line))

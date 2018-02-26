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

;; navigation and edition
(defhydra hydra-navigation (:pre (set-cursor-color "#40e0d0")
                            :post (progn
                                 (set-cursor-color "#ffffff")
                                 (message
                                  "Thank you, come again.")))
  "navigation"
  ;; kill line
  ;; kill word front
  ;; kill word back
  ;; begining of line
  ;; end of line
  ;; comment
  ;; move line
  ;; select start
  ;; word forward
  ;; word backward
  ;; begining of a file
  ;; end of file
  
  (";" forward-char "forward")
  (":" forward-word "forward word")
  ("j" backward-char "backward")
  ("J" backward-word "backward word")
  ("l" next-line "next")
  ("L" avy-goto-line-below "next few")
  ("k" previous-line "previous")
  ("K" avy-goto-line-above "previous few")
  ("o" sp-backward-sexp "back_paren")
  ("p" sp-forward-sexp "forward_paren")
  ("g" goto-line "goto line")
  ("s" avy-goto-subword-1 "goto char")
  ("." etags-select-find-tag-at-point "tag at point")
  ("f" etags-select-find-tag "tag by name")
  ("c" goto-last-change "goto last change")
  ("v" toggle-comment-on-line "comment/uncomment current line")
  
  ("i" nil "quit"))

(global-set-key (kbd "C-c n") 'hydra-navigation/body)
(key-chord-define-global "fj" 'hydra-navigation/body)

;; window manipulation
;; new window verticallyp
;; new window horizontlly
;; rezise left/right
;; resize up/down
;; maximalize window temporarly
;; (key-chord-define-global "gh" 'ace-window)
;; (key-chord-define-global "jf" 'ace-swap-window)


;; buffers


(global-set-key
 (kbd "C-h")
 (defhydra hydra-highlight ()
   "highlight"
   ("n" highlight-symbol-next "next")
   ("p" highlight-symbol-prev "previous")
   ("q" nil "quit")

   ))

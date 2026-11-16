;;; Complete Meow configuration mimicking Emacs vanilla keybinds.
((motion
  ("b" . meow-left)
  ("n" . meow-next)
  ("p" . meow-prev)
  ("f" . meow-right)
  ("<escape>" . ignore)
  )
 (leader
  ;; Canonical digit arguments
  ("1" . meow-digit-argument)
  ("2" . meow-digit-argument)
  ("3" . meow-digit-argument)
  ("4" . meow-digit-argument)
  ("5" . meow-digit-argument)
  ("6" . meow-digit-argument)
  ("7" . meow-digit-argument)
  ("8" . meow-digit-argument)
  ("9" . meow-digit-argument)
  ("0" . meow-digit-argument)
  ;; Discovery & Helpers
  ("/" . meow-keypad-describe-key)
  ("?" . meow-cheatsheet)
  )
 (normal
  ;; Numeric Expansions
  ("0" . meow-expand-0)
  ("1" . meow-expand-1)
  ("2" . meow-expand-2)
  ("3" . meow-expand-3)
  ("4" . meow-expand-4)
  ("5" . meow-expand-5)
  ("6" . meow-expand-6)
  ("7" . meow-expand-7)
  ("8" . meow-expand-8)
  ("9" . meow-expand-9)
  ("-" . negative-argument)

  ;; Text-Object & Bounds Modifiers
  (";" . meow-reverse)
  ("'" . meow-inner-of-thing)
  ("\"" . meow-bounds-of-thing)
  ("[" . meow-beginning-of-thing)
  ("]" . meow-end-of-thing)
  ("/" . meow-visit)
  ("." . repeat)

  ;; Core Navigation (Evil HJKL + Selection Expansion)
  ("b" . meow-left)
  ("B" . meow-left-expand)
  ("n" . meow-next)
  ("N" . meow-next-expand)
  ("p" . meow-prev)
  ("P" . meow-prev-expand)
  ("f" . meow-right)
  ("F" . meow-right-expand)

  ;; Mode Transitions & Open Lines
  ("a" . meow-append)
  ("i" . meow-insert)
  ("o" . meow-open-below)
  ("O" . meow-open-above)

  ;; Word & Symbol Navigation
  ;; TODO: should this be "e"?
  ("{" . meow-back-word)
  ("<" . meow-back-symbol)
  ("}" . meow-next-word)
  (">" . meow-next-symbol)
  ("m" . meow-mark-word)
  ("M" . meow-mark-symbol)

  ;; Deletion & Editing
  ("c" . meow-change)
  ("d" . meow-delete)
  ("D" . meow-backward-delete)
  ("k" . meow-kill)
  ("r" . meow-replace)

  ;; Find & Search
  ;; TODO: do I need the expand variants?
  ("t" . meow-till)
  ("T" . meow-till-expand)
  ("s" . meow-find)
  ("S" . meow-find-expand)
  ("?" . meow-search)

  ;; Line & Block Selections
  ("l" . meow-line)
  ("L" . meow-goto-line)
  ("x" . meow-block)
  ("X" . meow-to-block)
  ("j" . meow-join)

  ;; Clipboard & Multi-Cursor (Beacon Mode)
  ("w" . meow-save)
  ("W" . meow-sync-grab)
  ("y" . meow-yank)
  ("Y" . meow-yank-pop)
  ("g" . meow-grab)
  ("G" . meow-swap-grab)

  ;; Undo / History
  ("u" . meow-undo)
  ("U" . meow-undo-in-selection)

  ;; Miscellaneous / State Controls
  ("q" . meow-quit)
  ("<escape>" . meow-cancel-selection)
  ("`" . meow-pop-selection)))

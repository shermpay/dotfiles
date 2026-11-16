;;; Complete Meow configuration compatible with Colemak-DH and QWERTY.
;;; Retains Evil-style HJKL navigation while binding all canonical Meow commands.
((motion
  ("h" . meow-left)
  ("j" . meow-next)
  ("k" . meow-prev)
  ("l" . meow-right)
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
  ("," . meow-inner-of-thing)
  ("." . meow-bounds-of-thing)
  ("[" . meow-beginning-of-thing)
  ("]" . meow-end-of-thing)
  ("/" . meow-visit)
  ("'" . repeat)

  ;; Core Navigation (Evil HJKL + Selection Expansion)
  ("h" . meow-left)
  ("H" . meow-left-expand)
  ("j" . meow-next)
  ("J" . meow-next-expand)
  ("k" . meow-prev)
  ("K" . meow-prev-expand)
  ("l" . meow-right)
  ("L" . meow-right-expand)

  ;; Mode Transitions & Open Lines
  ("a" . meow-append)
  ("A" . meow-open-below)
  ("i" . meow-insert)
  ("I" . meow-open-above)

  ;; Word & Symbol Navigation
  ("b" . meow-back-word)
  ("B" . meow-back-symbol)
  ("e" . meow-next-word)
  ("E" . meow-next-symbol)
  ("w" . meow-mark-word)
  ("W" . meow-mark-symbol)

  ;; Deletion & Editing
  ("c" . meow-change)
  ("d" . meow-delete)
  ("D" . meow-backward-delete)
  ("s" . meow-kill)
  ("r" . meow-replace)

  ;; Find & Search
  ("f" . meow-find)
  ("t" . meow-till)
  ("n" . meow-search)

  ;; Line & Block Selections
  ("g" . meow-line)
  ("G" . meow-goto-line)
  ("o" . meow-block)
  ("O" . meow-to-block)
  ("m" . meow-join)

  ;; Clipboard & Multi-Cursor (Beacon Mode)
  ("y" . meow-save)
  ("Y" . meow-sync-grab)
  ("p" . meow-yank)
  ("P" . meow-yank-pop)
  ("x" . meow-grab)
  ("R" . meow-swap-grab)

  ;; Undo / History
  ("u" . meow-undo)
  ("U" . meow-undo-in-selection)

  ;; Miscellaneous / State Controls
  ("v" . meow-visit)
  ("q" . meow-quit)
  ("<escape>" . meow-cancel-selection)
  ("~" . meow-cancel-selection)
  ("z" . meow-pop-selection)
  ))

;; Local Variables:
;; after-save-hook: meow-config-load
;; End:

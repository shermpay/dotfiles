(fset 'koans-next-blank
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("_\262" 0 "%d")) arg)))

(defun timestamp ()
  "Add a timestamp to the current buffer"
  (interactive)
  (insert (format-time-string "%B %d, %Y\n")))

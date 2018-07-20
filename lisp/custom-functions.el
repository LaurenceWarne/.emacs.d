(defun lw-backward-brace ()
  (interactive)
  (progn
    ;; check if previous letter is a '{'
    (when (equal ?{ (char-before))
      (backward-char)
      )
    (skip-chars-backward "^{")
    )
  )

(defun lw-forward-brace ()
  (interactive)
  (progn
    (skip-chars-forward "^{")
    (forward-char)
    )
  )

(defun lw-save-line ()
  (interactive)
  (progn
    (move-beginning-of-line nil)
    (set-mark-command nil)
    (move-end-of-line nil)
    (kill-ring-save)))

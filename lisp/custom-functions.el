(defun lw-backward-brace ()
  (interactive)
  (progn
    ;; check if previous letter is a '{'
    (when (equal ?{ (char-before))
      (backward-char))
    (skip-chars-backward "^{")))

(defun lw-forward-brace ()
  (interactive)
  (progn
    (skip-chars-forward "^{")
    (forward-char)))

(defun lw-copy-text-to-next-line ()
  (interactive)
  (progn
    (setq contents-of-line (thing-at-point 'line t))
    ; Remove newline if it exists
    (if (equal "\n" (substring contents-of-line -1))
	(setq contents-of-line (substring contents-of-line 0 -1)))
    (end-of-line)
    (open-line 1)
    (next-line)
    (insert contents-of-line)
    (previous-line 1)
    (end-of-line)))

(defun lw-unix-line-discard()
  (interactive))

;;;; custom-functions.el -- useful interactive functions
;;; Commentary:

;; Interactive functions made by me! All are prefixed by lw- for namespacing

;;;; Code:


(defun lw-backward-brace ()
  "Move cursor to the first brace above in the current buffer, if it exists."
  (interactive)
  (progn
    ;; check if previous letter is a '{'
    (when (equal ?{ (char-before))
      (backward-char))
    (skip-chars-backward "^{")))

(defun lw-forward-brace ()
  "Move cursor to the first brace below in the current buffer, if it exists."
  (interactive)
  (progn
    (skip-chars-forward "^{")
    (forward-char)))

(defun lw-copy-text-to-next-line ()
  "Create a new line below the current line with the same contents as the current line. Does not affect the kill-ring."
  (interactive)
  (let ((contents-of-line (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position))))
    (end-of-line)
    (open-line 1)
    (next-line)
    (insert contents-of-line)
    (previous-line 1)
    (end-of-line)))

(defun lw-unix-line-discard()
  (interactive))

(defun lw-switch-to-last-buffer()
  "Switch to buffer returned by (other-buffer)."
  (interactive)
  (switch-to-buffer nil))

(defun newline-and-indent-ignoring-current-line()
  "Create a new line below the current line and go to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

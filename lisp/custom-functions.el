;;;; custom-functions.el -- useful interactive functions
;;; Commentary:

;; Functions made by me! All are prefixed by lw- for namespacing

;;;; Code:


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
  (interactive)
  (switch-to-buffer nil))

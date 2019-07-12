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
  "Create a new line below the current line with the same contents as the current line. Does not affect the kill ring."
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
  (interactive)
  (back-to-indentation)
  (kill-line))

(defun lw-switch-to-last-buffer()
  "Switch to buffer returned by (other-buffer)."
  (interactive)
  ;; Check here if line is empty
  (switch-to-buffer nil))

(defun newline-and-indent-ignoring-current-line()
  "Create a new line below the current line and go to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun lw-java-goto-test(&optional other-buffer)
  "Attempt to jump to the test file corresponding to the java file in the current buffer."
  (interactive)
  (let* ((test-dir (replace-regexp-in-string "/main/" "/test/" (buffer-file-name)))
	 (test-file (replace-regexp-in-string "\.java$" "Test.java" test-dir)))
    (if other-buffer
	(progn
	  (other-buffer) (find-file-at-point test-file) (other-buffer))
      (find-file-at-point test-file))))

(defun lw-java-goto-implementation(&optional other-buffer)
  "Attempt to jump to the implementation file corresponding to the java file in the current buffer."
  (interactive)
  (let* ((main-dir (replace-regexp-in-string "/test/" "/main/" (buffer-file-name)))
	 (main-file (replace-regexp-in-string "Test\.java$" ".java" main-dir)))
    (if other-buffer
	(progn
	  (other-buffer) (find-file-at-point main-file) (other-buffer))
      (find-file-at-point main-file))))

(defun lw-java-toggle-test-implementation(&optional other-buffer)
  "Goto test file if in implementation file, and implementation file if in a test file."
  (interactive)
  (if (string-match ".*/main/.*" (buffer-file-name))
      (lw-java-goto-test other-buffer)
    (lw-java-goto-implementation other-buffer)))

(defun lw-copy-to-next-line-region ()
  (interactive)
  (let* ((mark-start (region-beginning))
	 (mark-end (region-end))
	 (region-string (buffer-substring-no-properties mark-start mark-end)))
    (end-of-line)
    ;; If we are not already on a new line, create a new line
    (let ((contents-of-line (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position))))
      (when (not (string-match "^$" contents-of-line))
	  (newline)))
    (insert region-string)
    (goto-char mark-end)
    ;; There's an issue here with the marked region going invisible even with
    ;; Transient-Mark mode active
    (set-mark-command mark-start)))

(defun lw-copy-to-next-line-region-or-text ()
  (interactive)
  (if (use-region-p)
      (lw-copy-to-next-line-region)
    (lw-copy-text-to-next-line)))

(defun lw-test ()
  (if (looking-at "$")
      (message "match!")
    (message "no match!"))

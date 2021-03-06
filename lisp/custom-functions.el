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
  (skip-chars-forward "^{")
  (forward-char))

(defun lw-copy-text-to-next-line ()
  "Create a new line below the current line with the same contents as the current line.  Does not affect the kill ring."
  (interactive)
  (save-excursion
    (let ((contents-of-line (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position))))
      (end-of-line)
      (open-line 1)
      (forward-line)
      (insert contents-of-line))))

(defun lw-unix-line-discard()
  "Operates like ctrl-u on a unix terminal."
  (interactive)
  (back-to-indentation)
  (kill-line))

(defun lw-newline-and-indent-ignoring-current-line()
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

(defun lw-avy-swap-lines ()
  (interactive)
  (when (use-region-p)
    (let ((avy-all-windows nil)
          (fst-line-point (avy--line nil (region-beginning) (region-end))))
      (when fst-line-point
        ;; Highlight it
        (let ((snd-line-point (avy--line nil (region-beginning) (region-end))))
          (when snd-line-point
            (save-mark-and-excursion
              (push-mark fst-line-point)
              (goto-char snd-line-point)
              (transpose-lines 0))
            (lw-avy-swap-lines)))))))

(defvar lw-loading-screen-continue-thread t)

(defun lw-init-loading-screen ()
  (require 'snow)
  (let-it-snow)
  (switch-to-buffer "*snow*")
  (while lw-loading-screen-continue-thread
    (print (buffer-name (current-buffer)))
    (if (not (string= "*snow*" (buffer-name (current-buffer))))
        (progn
          (message "Switching to snow")
          (switch-to-buffer "*snow*"))
      (message "Buffer is correct"))))

(defun lw-elpy-tox-virtualenv ()
  (interactive)
  (let ((tox-dir (concat (projectile-project-root) ".tox/")))
    (message "Found tox directory: %s" tox-dir)
    (if (file-directory-p tox-dir)
        (let ((dir (read-directory-name "directory:" tox-dir)))
          (pyvenv-activate dir)
          (elpy-rpc-restart))
      (message "No .tox directory found at the project root."))))

(defun lw-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

; Copied from somewhere
(defun lw-jvm-get-file-package (&optional directory)
  (mapconcat 'identity
             (split-string
              (replace-regexp-in-string ".*src\\(/\\(main\\|test\\)\\)?\\(/scala\\)?"
                                        "" (or directory default-directory)) "/" t) "."))

(defun lw-delete-compilation-window ()
  "Delete any active compilation windows."
  (interactive)
  (mapc (lambda (window)
          (when-let* ((buf (window-buffer window))
                      ((compilation-buffer-p buf)))
            (message "Killing Window: %s" window)
            (delete-window window)
            (kill-buffer buf)))
        (window-list)))

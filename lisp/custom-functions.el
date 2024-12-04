;;;; custom-functions.el -- useful interactive functions -*- lexical-binding: t -*-
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

(defvar-local lw-unix-line-discard-bol-fn #'back-to-indentation)

(defun lw-unix-line-discard()
  "Operates like ctrl-u on a unix terminal."
  (interactive)
  (funcall lw-unix-line-discard-bol-fn)
  (kill-line))

(defun lw-newline-and-indent-ignoring-current-line()
  "Create a new line below the current line and go to it."
  (interactive)
  (end-of-line)
  (newline-and-indent))

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
                      ((or (string= (buffer-name buf) "*Async Shell Command*")
                           (compilation-buffer-p buf))))
            (message "Killing Window: %s" window)
            (delete-window window)
            (kill-buffer buf)))
        (window-list)))

(defun lw-open-line (n)
  "Open line then indent it."
  (interactive "*p")
  (open-line n)
  (save-excursion
    (forward-line 1)
    (indent-for-tab-command)))

(defun lw-popup-messages ()
  "Popup the messages buffer below the current."
  (interactive)
  (select-window (split-window-below))
  (switch-to-buffer "*Messages*")
  (let ((map (current-local-map)))
    (use-local-map (copy-keymap map))
    (local-set-key (kbd "q") 'delete-window)
    (local-set-key (kbd "k") 'delete-window)
    (local-set-key (kbd "C-d") 'delete-window)))

(defun lw-newline-smart-indent ()
  (interactive)
  (if (ignore-errors
        (save-excursion (re-search-forward
                         (rx (seq point (* blank) (any ?\] ?\) ?\] ?}))))))
      (progn (newline-and-indent)
             (newline-and-indent)
             (forward-line -1)
             (indent-according-to-mode))
    (newline-and-indent)))

(defun lw-repeat-complex-command (arg)
  (interactive "p")
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (&rest _)
               (cl-find-if (lambda (c) (not (eq 'lw-repeat-complex-command (car c))))
                           command-history))))
    (repeat-complex-command arg)))

;; Edited from https://www.reddit.com/r/emacs/comments/xdw6ok/comment/iodig8c
(defun lw-open-on-github ()
  (interactive)
  (let
      ((repo-url (magit-git-string "remote" "get-url" "--push" "origin"))
       (commit-hash (magit-git-string "rev-parse" "HEAD"))
       (start-line (if (use-region-p)
                       (line-number-at-pos (region-beginning))
                     (line-number-at-pos)))
       (end-line (if (use-region-p) (line-number-at-pos (region-end)))))
    (unless repo-url (error  "not in a git repo"))
    (browse-url
     (string-replace
      "ssh://git@"
      "https://"
      (concat
       (replace-regexp-in-string (rx (seq ".com" eos)) "" repo-url)
       "/blob/"
       commit-hash
       "/"
       (substring buffer-file-name (length (projectile-project-root)))
       "#L" (number-to-string start-line)
       (when (and (use-region-p) (< 0 (- end-line start-line)))
         (concat "..L" (number-to-string end-line))))))))

(defun increment-number-at-point ()
  "Increment the number at point."
  (interactive)
  (skip-chars-backward "0-9")
  (unless (looking-at "[0-9]+")
    (error "No number at point"))
  (let* ((n-str (match-string 0))
         (n (string-to-number n-str))
         (rep (number-to-string (1+ n)))
         (padding (max 0 (- (length n-str) (length rep))))
         (padded-rep (concat (make-string padding ?0) rep)))
    (replace-match padded-rep)))

(defun lw-flex ()
  "Perform smart flexing at point.

E.g. capitalize or decapitalize the next word, increment number at point."
  (interactive)
  (let ((case-fold-search nil))
    (call-interactively
     (cond ((looking-at "[0-9]+") #'increment-number-at-point)
           ((looking-at "[[:lower:]]") #'capitalize-word)
           ((looking-at "==") (delete-char 1) (insert "!") (forward-char 2))
           ((looking-at "!=") (delete-char 1) (insert "=") (forward-char 2))
           ((looking-at "+") (delete-char 1) (insert "-") (forward-char 1))
           ((looking-at "-") (delete-char 1) (insert "+") (forward-char 1))
           ((looking-at "<=") (delete-char 2) (insert ">=") (forward-char 2))
           ((looking-at ">=") (delete-char 2) (insert "<=") (forward-char 2))
           ((looking-at "<") (delete-char 1) (insert ">") (forward-char 1))
           ((looking-at ">") (delete-char 1) (insert "<") (forward-char 1))
           ((looking-at "<-") (delete-char 1) (insert "=") (forward-char 1))
           (t #'downcase-word)))))

(defun lw-package-refresh-contents-async ()
  "Call `package-refresh-contents' asynchronously."
  (interactive)
  (package-refresh-contents t)
  (message "Refreshing packages asynchronously..."))

(defun lw-copy-current-file ()
  "Copy the current file A.ext to a prompted dest B.ext, then replace occurrences of A with B in the new file."
  (interactive)
  (let* ((file (buffer-file-name))
         (from-string (file-name-nondirectory (file-name-sans-extension file)))
         (dest (read-file-name "Copy to: " default-directory nil nil (file-name-nondirectory file)))
         (to-string (file-name-nondirectory (file-name-sans-extension dest)))
         (dest-dir (file-name-directory dest))
         (contents (buffer-substring-no-properties (point-min) (point-max)))
         (edited-contents (with-temp-buffer
                            (insert contents)
                            (goto-char (point-min))
                            (message "Replacing occurrences of %s to %s in %s" from-string to-string dest)
                            (while (search-forward from-string nil t)
                              (replace-match to-string nil t))
                            (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (file-exists-p dest-dir) (make-directory dest-dir t))
    (find-file dest)
    (erase-buffer)
    (insert edited-contents)))

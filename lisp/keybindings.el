;;;; keybindings.el -- installs/initializes packages
;;; Commentary:

;; Here we set global keybindings, and local keybindings for vanilla
;; Emacs packages.  For local keybindings for external packages see
;; package-init.el.  Check keybindings active in the current buffer with
;; <C-h b>
;; See:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Keymaps.html

;;;; Code:

;;; Global keybindings

(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "M-j") 'lw-switch-to-last-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "C-q") 'query-replace)
(global-set-key (kbd "C-*") 'quoted-insert)
(global-set-key (kbd "C-+") 'electric-newline-and-maybe-indent)
(global-set-key (kbd "M-l") 'lw-copy-to-next-line-region-or-text)
(global-set-key (kbd "M-u") 'universal-argument)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "M-n") 'newline-and-indent-ignoring-current-line)
(global-set-key (kbd "M-;") 'eval-expression)
(global-set-key (kbd "M-'") 'mark-word)
(global-set-key (kbd "M-@") 'abbrev-prefix-mark)

;;; Local keybindings

;; Counter those annoying org keybindings
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-,") 'beginning-of-buffer)
	    (local-set-key (kbd "M-n") 'outline-next-heading)
	    (local-set-key (kbd "M-p") 'outline-previous-heading)
	    (local-set-key (kbd "M-[") 'org-backward-heading-same-level)
	    (local-set-key (kbd "M-]") 'org-forward-heading-same-level)))

(defvar org-delete-backward-char-custom-hook nil)

(defvar org-return-custom-hook nil)

(defun org-delete-backward-char-custom (N)
  "Call (org-delete-backward-char N) and then run hooks in org-delete-backward-char-custom-hook."
  (interactive "p")
  (progn
    (org-delete-backward-char N)
    (run-hooks 'org-delete-backward-char-custom-hook)))

(defun org-return-custom (&optional indent)
  "Call (org-return INDENT) and then run hooks in org-return-custom-hook."
  (interactive)
  (progn
    (org-return indent)
    (run-hooks 'org-return-custom-hook)))

;; https://www.reddit.com/r/emacs/comments/9h44lk/i_can_finally_preview_latex_in_orgmode_took_me/e69nfnw/
(defun org-auto-latex (&optional require-space)
  "Toggle display of latex fragments intelligently.
Toggle display of latex fragments if the cursor is preceded by a valid latex expression.  REQUIRE-SPACE further requires that the latex expression must be followed by whitespace or a period/comma."
  (save-excursion
    (when (and
	   (or (string-match-p "[,.[:blank:]\n]" (char-to-string (char-before)))
	       (not require-space))
	   (not (org-inside-LaTeX-fragment-p))
	   (progn (backward-char
		   (if require-space 2 1))
		  (org-inside-LaTeX-fragment-p)))
      (org-toggle-latex-fragment))))

(add-hook 'org-mode-hook
          (lambda ()
	    ;; Default to normal emacs line wrapping behaviour
	    (setq org-startup-truncated nil)
	    (org-remap org-mode-map
		       'delete-backward-char 'org-delete-backward-char-custom
		       'org-return 'org-return-custom)
	    ;; Render all latex in the buffer
	    (org-toggle-latex-fragment '(16))
            (add-hook 'post-self-insert-hook
		      (lambda () (org-auto-latex t)) 'append 'local)
            (add-hook 'org-delete-backward-char-custom-hook #'org-auto-latex)
            (add-hook 'org-return-custom-hook (lambda () (org-auto-latex t)))))

(add-hook 'java-mode-hook
          (lambda ()
	    (local-set-key (kbd "M-j") nil)  ; Will now default to global map
	    (local-set-key (kbd "M-k") 'lw-java-toggle-test-implementation)))

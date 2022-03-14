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
(global-set-key (kbd "M-n") 'lw-newline-and-indent-ignoring-current-line)
(global-set-key (kbd "C-u") 'lw-unix-line-discard)
(global-set-key (kbd "M-;") 'eval-expression)
(global-set-key (kbd "M-@") 'abbrev-prefix-mark)
(global-set-key (kbd "C-x C-o") 'find-file-other-window)
(global-set-key (kbd "C-x /") 'comment-dwim)
(global-set-key (kbd "C-x f") 'lw-put-file-name-on-clipboard)
(global-set-key (kbd "C-c k") 'lw-delete-compilation-window)
(global-set-key (kbd "C-M-f") 'forward-word)
(global-set-key (kbd "C-M-b") 'backward-word)
(global-set-key (kbd "C-o") 'lw-open-line)
(global-set-key (kbd "C-c m") 'lw-popup-messages)
;; (global-set-key (kbd "C-x w") 'buffer-file-name)

;;; Local keybindings

(add-hook 'c-mode-hook
          (lambda ()
	    (local-set-key (kbd "C-c C-c") nil)))

(add-hook 'read-only-mode-hook
          (lambda ()
            (let ((map (current-local-map)))
              (use-local-map (copy-keymap map))
              (local-set-key (kbd "k") 'kill-this-buffer)
              (local-set-key (kbd "q") 'kill-this-buffer))))

(add-hook 'java-mode-hook
          (lambda ()
	    (local-set-key (kbd "M-j") nil)))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (local-set-key (kbd "b") 'dired-up-directory)
	    (local-set-key (kbd "k") 'kill-this-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l") #'eshell/clear)))

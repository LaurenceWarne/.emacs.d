;;;; keybindings.el -- installs/initializes packages
;;; Commentary:

;; Here we set global keybindings, and local keybindings for vanilla
;; emacs packages. For local keybindings for external packages see
;; package-init.el. Check keybindings active in the current buffer with
;; <C-h b>
;; See:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Keymaps.html

;;;; Code:

;;; Global keybindings

(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-;") 'avy-goto-char-2)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "M-j") 'lw-switch-to-last-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "C-q") 'query-replace)
(global-set-key (kbd "C-'") 'goto-last-change)
(global-set-key (kbd "C-*") 'quoted-insert)
(global-set-key (kbd "C-+") 'electric-newline-and-maybe-indent)
(global-set-key (kbd "M-l") 'lw-copy-text-to-next-line)
(global-set-key (kbd "M-u") 'universal-argument)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "M-n") 'newline-and-indent-ignoring-current-line)

;;; Local keybindings

;; Counter those annoying additional org hooks
(add-hook 'org-mode-hook
          (lambda ()
	    (local-set-key (kbd "C-,") nil)
	    (local-set-key (kbd "C-j") nil)
            (local-set-key (kbd "C-,") 'beginning-of-buffer)
	    (local-set-key (kbd "C-j") 'helm-buffers-list)
	    (local-set-key (kbd "M-n") 'outline-next-heading)
	    (local-set-key (kbd "M-p") 'outline-previous-heading)
	    (local-set-key (kbd "M-[") 'org-backward-heading-same-level)
	    (local-set-key (kbd "M-]") 'org-forward-heading-same-level)))

(add-hook 'java-mode-hook
          (lambda ()
	    (local-set-key (kbd "M-j") nil)  ; Will now default to global map
	    (local-set-key (kbd "C-j") nil)
	    (local-set-key (kbd "M-k") nil)
	    (local-set-key (kbd "C-j") 'helm-projectile)
	    (local-set-key (kbd "M-k") 'lw-java-toggle-test-implementation)))

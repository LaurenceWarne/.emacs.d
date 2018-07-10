;;; init.el -- my init file
;;; Commentary:

;; See:
;; https://github.com/baron42bba/.emacs.d/blob/master/bba.org

;;; Code:

;; init.el
;; global_minor_mode_init.el
;; package_init.el
;; custom_functions.el
;; package_hooks_init.el
;; global_keybindings_init.el

(global-subword-mode 1)    ;; Moves cursor inbetween camelCase words
(global-linum-mode 1)      ;; Show line numbers
(delete-selection-mode 1)  ;; Make typing delete/overwrites selected text
(show-paren-mode 1)        ;; Turn on bracket match highlight
(desktop-save-mode 1)      ;; Save and automatically load previous emacs session on start up
(set-face-attribute 'default nil :height 105)
(scroll-bar-mode -1)       ;; No scrollbar
(tool-bar-mode -1)         ;; Get rid of the tool bar
(menu-bar-mode -1)         ;; Get rid of the menu bar

(setq show-paren-style 'expression)            ;; Highlight text between parens

;; adding melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; Initialise elpy
(package-initialize)
(elpy-enable)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Tell emacs where is your personal elisp lib dir
;; put this nearer to front?
(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(require 'smartparens)
(smartparens-global-mode)


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


;; ;; Enable lua mode
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
;; (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;; ;; (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

(projectile-global-mode)
(helm-mode 1)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-;") 'avy-goto-char-2)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "C-q") 'query-replace)
(global-set-key (kbd "C-'") 'pop-global-mark)

(global-set-key (kbd "M-n") 'lw-forward-brace)
(global-set-key (kbd "M-p") 'lw-backward-brace)

(setq aw-keys '(?a ?s ?d ?f ?e ?i ?j ?k ?l))

;; Add melpa repository
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
(package-initialize)

;; Check if use-package is installed and install if it's not
;; Note it's a melpa package so this has to come after the melpa repository is added
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

;; lsp recommends we do this for some reason
(require 'cc-mode)

;; Install all packages if not already installed (use-package must still be called)
(setq use-package-always-ensure t)

;; see
;;https://github.com/jwiegley/use-package
;;https://jwiegley.github.io/use-package/keywords/

;; This package does what you think it does
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package ace-window
  :config
  (bind-key "M-o" 'ace-window))

(use-package elpy
  ;; Enable Elpy in all future Python buffers.
  :init (add-hook 'python-mode-hook #'elpy-enable)
  :config (setq elpy-rpc-python-command "python3"))

(use-package avy
  ;; Creates autoloads for those commands which defers loading of the module until they are used
  :commands (avy-goto-char avy-goto-char-2)
  :config
  (setq avy-keys-alist
      `((avy-goto-char-2 . (?a ?s ?d ?f ?j ?k ?l)))))

(use-package yasnippet
  :defer t
  :init
  (autoload 'yasnippet "yasnippet" nil t)
  :config
  (yas-global-mode 1)
  (setq yas-indent-line 'auto)
  (setq yas-also-auto-indent-first-line t))

(use-package smartparens
  :demand t
  :config)
  ;(smartparens-global-mode 1))

(use-package projectile
  :config
  (projectile-global-mode 1))

;; See http://tuhdo.github.io/helm-projectile.html
(use-package helm
  :demand t
  :bind
  ("M-y" . 'helm-show-kill-ring)
  ("C-x C-f" . 'helm-find-files)
  ("C-j" . 'helm-mini)
  ("C-x b" . 'helm-buffers-list)
  ("M-x" . 'helm-M-x)
  :config
  (helm-mode 1)
  ;; Makes helm-boring-file-regexp-list act as a .gitignore 
  (setq helm-ff-skip-boring-files t)  
  (define-key helm-map (kbd "C-,") 'helm-beginning-of-buffer)
  (define-key helm-map (kbd "C-.") 'helm-end-of-buffer))

(use-package helm-flx
  :after helm
  :config
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t ;; t by default
  	helm-flx-for-helm-locate t)) ;; nil by default

(use-package helm-projectile
  :after (helm projectile)
  :defer t
  :init
  (autoload 'helm-projectile "helm-projectile" nil t)
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm))

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package company
  :config)
  ;(add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck)

(use-package hydra)

(use-package groovy-mode
  :commands groovy-mode
  :init (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode)))

(use-package speed-type
  :commands (speed-type-text speed-type-region speed-type-buffer))

(use-package goto-last-change
  :commands goto-last-change)

(use-package java-snippets
  :after yasnippet)

(use-package lsp-mode)

(use-package company-lsp)

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point))

(use-package lsp-java
  :after lsp
  :config
  ;; Do we need to start the server (if not already running) here as well?
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-vmargs
  	'("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/laurencewarne/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.16.20/ac76d9b956045631d1561a09289cbf472e077c01/lombok-1.16.20.jar" "-Xbootclasspath/a:/home/laurencewarne/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.16.20/ac76d9b956045631d1561a09289cbf472e077c01/lombok-1.16.20.jar")))

(use-package helm-lsp
  :after lsp helm)

(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package doom-themes
  :hook
  ((find-file-hook after-revert-hook) . doom-buffer-mode-maybe)
  (ediff-prepare-buffer-hook . doom-buffer-mode)
  (minibuffer-setup-hook . doom-brighten-minibuffer)
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t  ; if nil, italics is universally disabled

	;; doom-one specific settings
	doom-one-brighter-modeline nil
	doom-one-brighter-comments nil)
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-one t))

;; We need to call M-x all-the-icons-install-fonts
;; See https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-mode-swap-bg))
            
(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package eyebrowse
  :config
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
  (define-key eyebrowse-mode-map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
  (define-key eyebrowse-mode-map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
  (define-key eyebrowse-mode-map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
  (define-key eyebrowse-mode-map (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

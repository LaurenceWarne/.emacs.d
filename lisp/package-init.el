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

(use-package elpy
  ;; Enable Elpy in all future Python buffers.
  :init (add-hook 'python-mode-hook #'elpy-enable)
  :config (setq elpy-rpc-python-command "python3"))

(use-package avy
  ;; Creates autoloads for those commands which defers loading of the module until they are used
  :commands (avy-goto-char avy-goto-char-2)
  :config
  (setq avy-keys-alist
      `((avy-goto-char-2 . (?a ?s ?d ?f ?j ?k ?l))))

(use-package smex
  :bind (
	 ("M-x" . 'smex)
	 ("M-X" . 'smex-major-mode-commands)
	  ;; This is the old M-x.
	 ("C-c C-c M-x" . 'execute-extended-command)))

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

(use-package helm
  :demand t
  :config
  (helm-mode 1))

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

(use-package company
  :config)
  ;(add-hook 'after-init-hook 'global-company-mode))

(use-package treemacs
  :defer t
  :init
  (autoload 'treemacs "treemacs" nil t))

(use-package treemacs-projectile
  :after (projectile treemacs)
  :defer t
  :init
  (autoload 'treemacs-projectile "treemacs-projectile" nil t))

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
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-vmargs
  	'("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/laurencewarne/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.16.20/ac76d9b956045631d1561a09289cbf472e077c01/lombok-1.16.20.jar" "-Xbootclasspath/a:/home/laurencewarne/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.16.20/ac76d9b956045631d1561a09289cbf472e077c01/lombok-1.16.20.jar")))

(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; mu4e setup and configuration
;; mu4e needs to be installed for this to work
;; See:
;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(use-package doom-themes
  :init
  ;; emacs25 has no color-themes variable
  (setq color-themes '())  
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; We need to call M-x all-the-icons-install-fonts
;; See https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

;; (use-package doom-modeline
;;   :defer t
;;   :config
;;   :hook (after-init . doom-modeline-init))

(use-package solaire-mode
  :init
  ;; brighten buffers (that represent real files)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; ...if you use auto-revert-mode:
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  ;; You can do similar with the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  ;; To enable solaire-mode unconditionally for certain modes:
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(setq mu4e-contexts
 `( ,(make-mu4e-context
     :name "Gmail"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
     :vars '(
       (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
       (mu4e-refile-folder . "/Gmail/[Gmail].Archive")
       ))
    ))

;; Hooks

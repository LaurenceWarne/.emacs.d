;;;; package-init.el -- installs/initializes packages
;;; Commentary:

;; Use use-package to install and configure packages in a readable way.
;; See:
;; https://github.com/jwiegley/use-package
;; https://jwiegley.github.io/use-package/keywords/

;;;; Code:

;;; Add repositories

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


;;; Use package declarations

;; Install all packages if not already installed (use-package must still be called)
(setq use-package-always-ensure t)

;; This package does what you think it does
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-prompt-before-update t)
  (auto-package-update-maybe))

(use-package ace-window
  :config
  (bind-key "M-o" 'ace-window))

;; NOTE:
;; for this package to work you need to run:
;; $ pip3 install jedi flake8 autopep8 black yapf --user
;; See:
;; https://github.com/jorgenschaefer/elpy
(use-package elpy
  ;; Enable Elpy in all future Python buffers.
  :init (elpy-enable)
  :config (setq elpy-rpc-python-command "python3"))

(use-package avy
  ;; This does two things: first, it creates an autoload for the avy-goto-char commands and defers loading of avy until you actually use it. Second, it binds the key C-: to that command.
  :bind
  ("C-:" . 'avy-goto-char)
  ("C-;" . 'avy-goto-char-2)
  ("C-#" . 'avy-copy-region)
  ("M-#" . 'avy-copy-line)
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
  :bind (:map smartparens-mode-map
	      ;; Unfortunately can't bind C-[ here as it's bound to ESC
	      ("M-[" . 'sp-backward-sexp)
	      ("M-]" . 'sp-forward-sexp))
  :config
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode))
  ;(smartparens-global-mode 1))

(use-package projectile
  :config
  (projectile-mode 1))

(use-package helm
  :demand t
  :bind
  ("M-y" . 'helm-show-kill-ring)
  ("C-x C-f" . 'helm-find-files)
  ("C-j" . 'helm-mini)
  ("C-x b" . 'helm-buffers-list)
  ("M-x" . 'helm-M-x)
  ("C-s" . 'helm-occur)
  :config
  (helm-mode 1)
  ;; Makes helm-boring-file-regexp-list act as a .gitignore
  (setq helm-ff-skip-boring-files t)
  (setq helm-M-x-fuzzy-match t)
  (define-key helm-map (kbd "C-,") 'helm-beginning-of-buffer)
  (define-key helm-map (kbd "C-.") 'helm-end-of-buffer)
  (define-key helm-map (kbd "C-k") 'helm-buffer-run-kill-buffers)
  (define-key helm-map (kbd "M-D") 'helm-delete-minibuffer-contents)
  (define-key helm-occur-map (kbd "C-s") 'helm-next-line)
  (define-key helm-occur-map (kbd "C-r") 'helm-previous-line)
  (require 'org)
  (define-key org-mode-map (kbd "C-j") 'helm-mini))

(use-package helm-flx
  :after helm
  :config
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t ; t by default
  	helm-flx-for-helm-locate t))   ; nil by default

(use-package helm-ag
  :after helm
  :config
  (define-key java-mode-map (kbd "M-s") 'helm-ag-edit))

(use-package helm-projectile
  :after (helm projectile helm-ag groovy-mode)
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (define-key java-mode-map (kbd "C-j") 'helm-projectile)
  (define-key java-mode-map (kbd "M-q") 'helm-projectile-ag)
  ;; Project integration as we mostly use groovy for gradle config
  (define-key groovy-mode-map (kbd "C-j") 'helm-projectile)
  (define-key groovy-mode-map (kbd "M-q") 'helm-projectile-ag))

(use-package helm-descbinds
  :after helm
  :defer t
  :bind (("C-h b" . helm-descbinds)
	 ("C-h w" . helm-descbinds)))

(use-package company
  :config
  ;; We usually want make sure we have appropriate backends before enabling
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

(use-package flycheck
  ;; Don't use :hook here as that defers loading until flycheck is called
  :config (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

(use-package hydra)

(use-package groovy-mode
  :init (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode)))

(use-package speed-type
  :commands (speed-type-text speed-type-region speed-type-buffer))

(use-package goto-last-change
  :bind
  ("C-'" . 'goto-last-change))

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
  :after lsp-mode
  :config
  (setq lsp-java-format-comments-enabled nil)
  (setq lsp-java-format-on-type-enabled nil)
  ;; Do we need to start the server (if not already running) here as well?
  (add-hook 'java-mode-hook 'lsp)
  (add-hook
   'java-mode-hook
   (lambda ()
     (add-hook 'before-save-hook 'lsp-java-organize-imports nil 'local)))
  (setq lsp-java-vmargs
	;; Needs lombok in the gradle cache
  	'("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/laurencewarne/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.8/448003bc1b234aac04b58e27d7755c12c3ec4236/lombok-1.18.8.jar" "-Xbootclasspath/a:/home/laurencewarne/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.8/448003bc1b234aac04b58e27d7755c12c3ec4236/lombok-1.18.8.jar"))
  (setq tab-width 4))

(use-package helm-lsp
  :after lsp helm)

(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package doom-themes
  :hook
  ((find-file-hook after-revert-hook) . doom-buffer-mode-maybe)
  (ediff-prepare-buffer-hook . doom-buffer-mode)
  (minibuffer-setup-hook . doom-brighten-minibuffer)
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t  ; if nil, italics is universally disabled

	;; doom-one specific settings
	doom-one-brighter-modeline nil
	doom-one-brighter-comments nil)
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-one t))

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-mode-swap-bg))
            
(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-elisp-mode-hook 'rainbow-delimiters-mode))

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

;; https://github.com/Malabarba/camcorder.el
(use-package camcorder
  :commands camcorder-mode)

;; https://github.com/purcell/package-lint
(use-package package-lint)

;; https://github.com/LaurenceWarne/jdoc-jumper
(use-package jdoc-jumper
  :commands jdoc-jumper-jump-from-point
  :load-path "~/projects/jdoc-jumper")

;; https://github.com/politza/pdf-tools/blob/master/README.org
(use-package pdf-tools
  :config
  (pdf-loader-install)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-,") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "C-.") 'pdf-view-last-page)
  (define-key pdf-view-mode-map (kbd "C--") 'pdf-view-shrink)
  (define-key pdf-view-mode-map (kbd "C-=") 'pdf-view-enlarge)
  (set-face-attribute 'pdf-isearch-lazy nil
		      :inherit 'lazy-highlight
		      :foreground "black"
		      :background "grey")
  (set-face-attribute 'pdf-isearch-match nil
		      :inherit 'isearch
		      :foreground "white"
		      :background "black"))

;; https://github.com/anwyn/slime-company
(use-package slime-company
  ;; We have to call this before slime is loaded:
  ;; https://github.com/anwyn/slime-company/issues/11
  :init
  (slime-setup '(slime-fancy slime-company)))

(use-package slime
  :config
  ;; Set your lisp system and, optionally, some contribs
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  (define-key slime-repl-mode-map (kbd "M-,") 'slime-describe-symbol)
  (define-key slime-repl-mode-map (kbd "C-c C-d C-d") 'slime-pop-find-definition-stack))

(use-package steam
  :load-path "~/projects/steam.el"
  :config
  (setq steam-username "39422361280608732623190235")
  (setq org-startup-with-inline-images t))

(use-package beacon
  :config
  (beacon-mode 1))

;; Requires shellcheck:
;; https://github.com/koalaman/shellcheck
;; https://github.com/federicotdn/flymake-shellcheck
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

;; The emacs startup profiler
;; https://github.com/jschaf/esup
(use-package esup
  :commands esup)

(use-package lorem-ipsum
  :commands
  (lorem-ipsum-insert-sentences
   lorem-ipsum-insert-paragraphs
   lorem-ipsum-insert-list))

;; https://github.com/millejoh/emacs-ipython-notebook
(use-package ein)

;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :bind (:map paredit-mode-map
	      ("C-0" . 'paredit-forward-slurp-sexp)
	      ("C-9" . 'paredit-backward-slurp-sexp))
  :config
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook #'enable-paredit-mode))

;; https://github.com/vermiculus/sx.el
(use-package sx
  :config
  (bind-keys :prefix "C-c s"
             :prefix-map my-sx-map
             :prefix-docstring "Global keymap for SX."
             ("q" . sx-tab-all-questions)
             ("i" . sx-inbox)
             ("o" . sx-open-link)
             ("u" . sx-tab-unanswered-my-tags)
             ("a" . sx-ask)
             ("s" . sx-search)))

;; https://github.com/alphapapa/org-rifle
(use-package helm-org-rifle
  :config
  (require 'org)
  (define-key org-mode-map (kbd "M-r") #'helm-org-rifle-org-directory))

;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key))

;; https://magit.vc/
(use-package magit)

;; https://github.com/syohex/emacs-zoom-window
(use-package zoom-window
  :config
  (global-set-key (kbd "M-i") 'zoom-window-zoom))

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

;; Check if use-package is installed and install if its not
;; Note it's a melpa package so this has to come after the melpa repository is added
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

;; Install all packages if not already installed (use-package must still be called)
(setq use-package-always-ensure t)

;; see
;;https://github.com/jwiegley/use-package
;;https://jwiegley.github.io/use-package/keywords/

(use-package elpy
  ;; Enable Elpy in all future Python buffers.
  :init (add-hook 'python-mode-hook #'elpy-enable))


(use-package avy
  ;; Creates autoloads for those commands which defers loading of the module until they are used
  :commands (avy-goto-char avy-goto-char-2)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?e ?i ?j ?k ?l)))

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
  (yas-global-mode 1))

(use-package smartparens
  :demand t
  :config
  (smartparens-global-mode 1))

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
  :config
  (add-hook 'after-init-hook 'global-company-mode))

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

;; (use-package meghanada
;;   :when (= emacs-major-version 25)
;;   :config
;;   (progn
;;     (add-hook 'java-mode-hook
;; 	      (lambda ()
;; 		;; meghanada-mode on
;; 		(meghanada-mode t)
;; 		(flycheck-mode +1)
;; 		(setq c-basic-offset 2)
;; 		;; use code format
;; 		(add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
;;     (cond
;;      ((eq system-type 'windows-nt)
;;       (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
;;       (setq meghanada-maven-path "mvn.cmd"))
;;      (t
;;       (setq meghanada-java-path "java")
;;       (setq meghanada-maven-path "mvn")))))

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
  (package-install 'use-package))

;; Install all packages if not already installed (use-package must still be called)
(setq use-package-always-ensure t)

;; see
;;https://github.com/jwiegley/use-package
;;https://jwiegley.github.io/use-package/keywords/

(use-package elpy
  :defer t
  :init
  (autoload 'elpy "elpy" nil t)
  :config
  ;; Enable Elpy in all future Python buffers.
  (elpy-enable))

(use-package avy
  ;; Creates autoloads for those commands and defers loading of the module until they are used
  :commands (avy-goto-char avy-goto-char-2)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?e ?i ?j ?k ?l)))

(use-package yasnippet
  :defer t
  :init
  (autoload 'yasnippet "yasnippet" nil t)
  :config
  (yas-global-mode 1))

(use-package smartparens
  :defer t
  :init
  (autoload 'smartparens "smartparens" nil t)
  :config
  (smartparens-global-mode 1))

(use-package projectile
  :defer t
  :init
  (autoload 'projectile "projectile" nil t)
  :config
  (projectile-global-mode 1))

(use-package helm
  :defer t
  :init
  (autoload 'helm "helm" nil t)
  :config
  (helm-mode 1)
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :after (helm projectile)
  :defer t
  :init
  (autoload 'helm-projectile "helm-projectile" nil t)
  :config
  (helm-projectile-on))

(use-package company
  :defer t
  :init
  (autoload 'company "company" nil t)
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


  


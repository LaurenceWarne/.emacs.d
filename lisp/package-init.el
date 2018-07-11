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
(package-initialize)

;; Check if use-package is installed and install if its not, note it's a melpa package so
;; has to come after the melpa repository is added
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Initialise elpy
(elpy-enable)

(require 'yasnippet)
(yas-global-mode 1)

(require 'smartparens)
(smartparens-global-mode)

(projectile-global-mode)
(helm-mode 1)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(add-hook 'after-init-hook 'global-company-mode)

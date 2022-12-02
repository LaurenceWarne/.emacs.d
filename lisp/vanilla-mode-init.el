;;;; package-init.el -- initializes Emacs libs -*- lexical-binding: t -*-
;;; Commentary:

;; Use use-package to configure vanilla Emacs libraries and modes
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
  
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(package-initialize)

;; Check if use-package is installed and install if it's not
;; Note it's a melpa package so this has to come after the melpa repository is added
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

;;; Use package declarations

;; Install all packages if not already installed (use-package must still be called)
(setq use-package-always-ensure t)

(use-package proced
  :ensure nil
  :commands proced
  :config
  (setq-default proced-auto-update-flag t)
  (setq-default proced-auto-update-interval 1)
  (setq-default proced-goal-attribute nil)
  (setq proced-enable-color-flag t)
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem vsize rss start time state (args comm)))
  (setq-default proced-format 'custom))

(use-package dired
  :ensure nil
  :commands dired
  :bind (("C-M-d" . (lambda () (interactive) (dired default-directory)))
         :map dired-mode-map
         ("b" . dired-up-directory)
         ("k" . kill-this-buffer))
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode))
  :config
  (require 'dired-x))

(use-package python
  :ensure nil
  :bind (:map python-mode-map ("<C-return>" . lw-python-shell-send-region-or-line))
  :config
  (setq python-shell-interpreter "/usr/bin/python3")

  (defun lw-python-shell-send-buffer (&optional send-main msg)
    (interactive (list current-prefix-arg t))
    (unless (python-shell-get-process)
      (run-python)
      ;; Next one here caught me out for hours
      (sleep-for 0 100))
    (python-shell-send-buffer)
    (let ((buf (python-shell-get-buffer)))
      (if-let ((window (get-buffer-window buf)))
          (select-window window)
        (other-window 1)
        (switch-to-buffer buf)))
    (end-of-buffer))

  (defun python-shell-send-current-statement ()
    (interactive)
    (let ((beg (python-nav-beginning-of-statement))
          (end (python-nav-end-of-statement)))
      (python-shell-send-string (buffer-substring beg end)))
    (python-nav-forward-statement))

  (defun lw-python-shell-send-region-or-line ()
    (interactive)
    (cond ((region-active-p)
           (setq deactivate-mark t)
           (python-shell-send-region (region-beginning) (region-end)))
          (t (python-shell-send-current-statement))))

  ;; https://bmag.github.io/2015/12/26/desktop.html#supporting-more-buffer-types  
  (defun lw-save-python-buffer (desktop-dirname)
    default-directory)

  (defun lw-create-python-buffer (_file-name buffer-name misc)
    (let ((default-directory misc))
      (run-python)))

  ;; Save python buffers
  (add-hook 'inferior-python-mode-hook
	    (lambda () (setq-local desktop-save-buffer #'lw-save-python-buffer)))

  (add-to-list 'desktop-buffer-mode-handlers '(inferior-python-mode . lw-create-python-buffer)))

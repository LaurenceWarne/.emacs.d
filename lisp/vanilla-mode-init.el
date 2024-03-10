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

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;;; Use package declarations

;; Install all packages if not already installed (use-package must still be called)
(setq use-package-always-ensure t)

(use-package proced
  :ensure nil
  :commands proced
  :bind (("C-M-p" . proced)
         :map proced-mode-map
         ("k" . kill-this-buffer)
         ("q" . kill-this-buffer)
         ("K" . proced-send-signal))
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  (proced-auto-update-interval 2)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pgpu pmem rss vram start time state (args comm))))

(use-package desktop
  :ensure nil
  :config
  ;; Don't save to desktop third party lib files from metal's goto def
  (setq desktop-buffers-not-to-save-function
        (lambda (filename bufname mode &rest rest)
          (or (not (stringp filename))
              (not (string-match-p (rx (seq "/.metals/readonly/dependencies"))
                                   (file-name-directory filename)))))))

(use-package dired
  :ensure nil
  :commands dired
  :init
  (defun lw-dired (arg)
    (interactive "P")
    (let ((file (file-name-nondirectory (buffer-file-name))))
      (if (or (not (numberp (car-safe arg))) (/= (car-safe arg) 4))
          (dired default-directory)
        (let ((current-prefix-arg nil))
          (call-interactively #'dired)
          (re-search-forward file)))
      (re-search-forward file)))
  :bind (("C-M-d" . lw-dired)
         :map dired-mode-map
         ("b" . dired-up-directory)
         ("k" . kill-this-buffer))
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode))
  ;; args to 'ls'
  :custom
  (dired-listing-switches "-alFh")
  (dired-dwim-target t)
  :config
  (require 'dired-x)

  ;; https://stackoverflow.com/questions/12994164/allow-dired-do-copy-and-dired-do-rename-to-create-new-dir-on-the-fly
  (defadvice dired-mark-read-file-name (after rv:dired-create-dir-when-needed (prompt dir op-symbol arg files &optional default) activate)
    (when (member op-symbol '(copy move))
      (let ((directory-name (if (< 1 (length files))
                                ad-return-value
                              (file-name-directory ad-return-value))))
        (when (and (not (file-directory-p directory-name))
                   (y-or-n-p (format "directory %s doesn't exist, create it?" directory-name)))
          (make-directory directory-name t)))))

  ;; If dir is nil, causes errors when java sources are recreated from desktop
  ;; TODO use advice and/or investigate more
  (defun dired-omit-case-fold-p (dir)
    "Non-nil if `dired-omit-mode' should be case-insensitive in DIR."
    (and dir (if (eq dired-omit-case-fold 'filesystem)
                 (file-name-case-insensitive-p dir)
               dired-omit-case-fold))))

(use-package wdired
  :ensure nil
  :defer t
  :bind (:map wdired-mode-map ("C-x k" . wdired-abort-changes)))

(use-package python
  :ensure nil
  :bind (:map python-mode-map
              ("<C-return>" . lw-python-shell-send-region-or-line)
              ("C-c C-c" . lw-python-send-or-projectile))
  :config
  (setq python-shell-interpreter "/usr/bin/python3")

  (defvar-local lw-python-no-shell nil)
  (defun lw-python-send-or-projectile (&optional send-main msg)
    "Send/create shell or run tests for project."
    (interactive)
    (require 'projectile)
    (let ((type (projectile-project-type)))
      (if (or (eq type 'python-tox) (eq type 'python-poetry) lw-python-no-shell)
          (call-interactively #'projectile-test-project)
        (lw-python-shell-send-buffer send-main msg))))

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

;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(use-package eshell
  :ensure nil
  :defer t
  :config
  (setq eshell-hist-ignoredups 'erase)
  (defun lw-eshell-clear-buffer-or-recenter ()
    "Clear eshell buffer."
    (interactive)
    (if (eobp)
        (let ((inhibit-read-only t)
              (line (buffer-substring-no-properties
                     (progn (eshell-bol) (point))
	             (progn (end-of-line) (point)))))
          (erase-buffer)
          (eshell-send-input)
          (insert line))
      (recenter-top-bottom)))

  (defun lw-eshell-delete-char-or-exit (&optional killflag)
    "Call `delete-char' or exit the buffer + window if there is no forward char."
    (interactive)
    (condition-case nil
        (delete-char 1 killflag)
      (end-of-buffer
       (kill-buffer-and-window))))

  (defun lw-eshell-send-input-and-reinsert ()
    "Send input and reinsert the last command"
    (interactive)
    (eshell-send-input)
    (call-interactively #'eshell-previous-matching-input-from-input))
  ;; https://lists.gnu.org/r/bug-gnu-emacs/2019-06/msg01616.html
  (add-hook
   'eshell-mode-hook
   (lambda ()
     (define-key eshell-mode-map (kbd "C-l") #'lw-eshell-clear-buffer-or-recenter)
     (define-key eshell-mode-map (kbd "C-d") #'lw-eshell-delete-char-or-exit)
     (define-key eshell-mode-map (kbd "M-<RET>") nil)
     (define-key eshell-mode-map (kbd "<C-return>") #'lw-eshell-send-input-and-reinsert)
     (define-key eshell-hist-mode-map (kbd "M-s") nil))
   99))

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c C-c" . nil)
              ("M-q" . nil)
              ("<return>" . lw-newline-smart-indent)))

(use-package java-mode
  :ensure nil
  :hook ((java-mode . eclipse-indent-setup)
         (java-mode . dired-omit-mode))
  :init
  (defun eclipse-indent-setup ()
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)))

(use-package js
  :ensure nil
  :bind (:map js-mode-map
              ("C-c C-c" . nil)
              ("M-q" . nil)
              ("<return>" . lw-newline-smart-indent)))

(use-package comint
  :ensure nil
  :config
  (setq comint-input-ignoredups t))

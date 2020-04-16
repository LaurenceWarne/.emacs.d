;;;; global-minor-mode-init.el -- configure vanilla emacs modes
;;; Commentary:

;; No external package configuration is done here, see package-init.el
;; for that.

;;;; Code:


;;; Enable/disable modes
(global-subword-mode 1)    ; Moves cursor inbetween camelCase words
(delete-selection-mode 1)  ; Make typing delete/overwrites selected text
(show-paren-mode 1)        ; Turn on bracket match highlight
(desktop-save-mode 1)      ; Save and automatically load previous emacs session on start up
(electric-pair-mode 1)     ; Have that nice brace/parentheses auto indent
(set-face-attribute 'default nil :height 105)
(scroll-bar-mode -1)       ; No scrollbar
(tool-bar-mode -1)         ; Get rid of the tool bar
(menu-bar-mode -1)         ; Get rid of the menu bar

;;; Set variables
(setq visible-bell 1)                  ; Get rid of annoying bell, use flash screen instead
(setq show-paren-style 'parenthesis)   ; Highlight text between parens
(fset 'yes-or-no-p 'y-or-n-p)          ; Use y/n instead of yes/no
;; https://www.emacswiki.org/emacs/DisabledCommands
(setq disabled-command-function nil)   ; Enable all disabled commands

(require 'dired)
(require 'dired-x)
(add-hook 'dired-mode-hook (lambda ()
			     (dired-hide-details-mode 1)
			     (dired-omit-mode 1)))

(require 'org)
(setq org-use-speed-commands t)        ; Shortcut for org commands when on headlines
(setq org-startup-with-inline-images t)
(setq org-startup-folded nil)

(require 'python)
(setq python-shell-interpreter "/usr/bin/python3")


(defun eclipse-indent-setup ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0))
(add-hook 'java-mode-hook 'eclipse-indent-setup)

;;; Save special buffers
;; See:
;; https://bmag.github.io/2015/12/26/desktop.html#supporting-more-buffer-types
(defun lw-save-python-buffer (desktop-dirname)
  default-directory)

(defun lw-create-python-buffer (_file-name buffer-name misc)
  (let ((default-directory misc))
    (run-python)))

(defun lw-create-ielm-buffer (_file-name buffer-name misc)
  (let ((default-directory misc))
    ;; create a shell buffer named BUFFER-NAME in directory MISC
    (ielm)))

;; Save python buffers
(add-hook 'inferior-python-mode-hook
	  (lambda () (setq-local desktop-save-buffer #'lw-save-python-buffer)))

(add-to-list 'desktop-buffer-mode-handlers '(inferior-python-mode . lw-create-python-buffer))

;; Save ielm buffers
(add-hook 'ielm-mode-hook
	  (lambda () (setq-local desktop-save-buffer #'lw-save-python-buffer)))

(add-to-list 'desktop-buffer-mode-handlers '(inferior-emacs-lisp-mode . lw-create-ielm-buffer))

;; Enable collapsing of xml
;; see https://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question
(require 'nxml-mode)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)
(define-key nxml-mode-map (kbd "C-c e") 'hs-toggle-hiding)
(define-key nxml-mode-map (kbd "C-c C-e") 'hs-toggle-hiding)

(add-hook 'emacs-lisp-mode-hook (lambda () electric-pair-mode -1))

(defvar lw-read-the-org-setup-file "~/repos/org-html-themes/setup/theme-readtheorg.setup")

(require 'ox)
(org-export-define-derived-backend 'lw-wiki-html 'html
  :options-alist `((:setupfile "SETUPFILE" nil ,lw-read-the-org-setup-file t))
  :menu-entry
  '(?h "Export to HTML"
       ((?w "As a wiki file" lw-org-export-to-html))))

(defun lw-org-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Org export menu entry for read-the-org."
  (let* ((extension ".html")
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system 'utf-8))
    (org-export-to-file 'lw-wiki-html file
      async subtreep visible-only body-only ext-plist)))

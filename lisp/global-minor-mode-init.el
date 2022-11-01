;;;; global-minor-mode-init.el -- configure vanilla emacs modes
;;; Commentary:

;; No external package configuration is done here, see package-init.el
;; for that.

;;;; Code:

(require 'cl-lib)

;;; Enable/disable modes
(global-subword-mode 1)    ; Moves cursor inbetween camelCase words
(delete-selection-mode 1)  ; Make typing delete/overwrites selected text
(show-paren-mode 1)        ; Turn on bracket match highlight
(desktop-save-mode 1)      ; Save and automatically load previous emacs session on start up
(savehist-mode 1)          ; Save any variables added to `savehist-additional-variables'
(column-number-mode 1)
(global-auto-revert-mode 1)
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil
                      :height 100
                      :family "Bitstream Vera Sans Mono"
                      :weight 'normal
                      :width 'normal))
(scroll-bar-mode -1)       ; No scrollbar
(tool-bar-mode -1)         ; Get rid of the tool bar
(menu-bar-mode -1)         ; Get rid of the menu bar

;;; Set variables
;; for the following two, see https://emacs.stackexchange.com/questions/29768/how-to-stop-emacs-from-blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)
(setq show-paren-style 'parenthesis)   ; Highlight text between parens
(fset 'yes-or-no-p 'y-or-n-p)          ; Use y/n instead of yes/no
;; https://www.emacswiki.org/emacs/DisabledCommands
(setq disabled-command-function nil)   ; Enable all disabled commands
(setq mac-command-modifier 'meta)
(setq mac-right-option-modifier 'control)
(setq ns-use-native-fullscreen nil)    ; Fix Emacs jumping into its own workspace whenever it's fullscreened
(setq compilation-scroll-output t)
(setq confirm-kill-processes nil)      ; Don't ask for exit confirmation when there are subprocesses
(setq browse-url-browser-function 'browse-url-firefox)
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ; 1MB
(setq kill-do-not-save-duplicates t)
(setq history-delete-duplicates t)           ; Delete duplicate history elements
(setq enable-recursive-minibuffers t)
(when (file-exists-p "/var/tmp")
  (setq lock-file-name-transforms
        '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t))))  ; Make Emacs write all the lock files to /var/tmp/
(require 'holidays)
(setq calendar-holidays holiday-christian-holidays)

(require 'savehist)
(add-to-list 'savehist-additional-variables 'kill-ring)
(add-to-list 'savehist-additional-variables 'query-replace-history)

(require 'dired)
(require 'dired-x)
(add-hook 'dired-mode-hook (lambda ()
			     (dired-hide-details-mode 1)
			     (dired-omit-mode 1)))

(require 'python)
(setq python-shell-interpreter "/usr/bin/python3")

(defun eclipse-indent-setup ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0))
(add-hook 'java-mode-hook 'eclipse-indent-setup)
(add-hook 'java-mode-hook (lambda () (setq compile-command "javac *.java")))

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

(define-key python-mode-map (kbd "<C-return>") 'lw-python-shell-send-region-or-line)

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

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (electric-pair-local-mode -1)
            ;; https://github.com/bbatsov/emacs-lisp-style-guide
	    (setq indent-tabs-mode nil)))

(add-hook 'python-mode-hook (lambda () (local-set-key (kbd "<return>") #'lw-newline-smart-indent)))
(add-hook 'java-mode-hook (lambda () (local-set-key (kbd "<return>") #'lw-newline-smart-indent)))

;; Stolen from (http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html)
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'endless/colorize-compilation)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Advice

;; See also https://www.reddit.com/r/emacs/comments/rlli0u/whats_your_favorite_defadvice/

(defadvice kill-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

;; https://emacs.stackexchange.com/questions/73658/change-the-order-of-minor-modes-on-the-modeline
(add-hook
 'after-load-functions
 (prog1
     (defun lw-re-arrange-minor-mode-alist (&rest _)
       (cl-loop with modes =
                '(
                  ;; order in which you want the minor mode lighters to
                  ;; appear
                  projectile-mode
                  lsp-mode
                  flycheck-mode
                  smartparens-mode
                  ;; To get the current order of entries, and what to plug in
                  ;; here do `M-x pp-eval-expression RET (mapcar #'car minor-mode-alist)'
                  )
                for mode in (nreverse modes)
                for mode-line-entry = (assq mode minor-mode-alist)
                when mode-line-entry do
                (assq-delete-all mode minor-mode-alist)
                (add-to-list 'minor-mode-alist mode-line-entry)))
   (lw-re-arrange-minor-mode-alist)))

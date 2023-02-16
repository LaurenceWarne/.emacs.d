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
(when (and (eq system-type 'gnu/linux) (x-list-fonts "Iosevka"))
  (set-face-attribute 'default nil
                      :height 110
                      :family "Iosevka"
                      :weight 'normal
                      :width 'normal))
(scroll-bar-mode -1)       ; No scrollbar
(tool-bar-mode -1)         ; Get rid of the tool bar
(menu-bar-mode -1)         ; Get rid of the menu bar

;;; Set variables
;; for the following two, see https://emacs.stackexchange.com/questions/29768/how-to-stop-emacs-from-blinking
(setq visible-bell nil
      initial-major-mode #'org-mode          ; Prefer `org-mode' for *scratch*
      initial-scratch-message "* Scratch\n\n"
      ring-bell-function #'ignore
      show-paren-style 'parenthesis          ; Highlight text between parens
      disabled-command-function nil          ; Enable all disabled commands
      ;; Mac OS stuff
      mac-command-modifier 'meta
      mac-right-option-modifier 'control
      ns-use-native-fullscreen nil           ; Fix Emacs jumping into its own workspace whenever it's fullscreened
      compilation-scroll-output t
      confirm-kill-processes nil             ; Don't ask for exit confirmation when there are subprocesses
      browse-url-browser-function 'browse-url-firefox
      gc-cons-threshold 100000000           ; https://emacs-lsp.github.io/lsp-mode/page/performance/
      read-process-output-max (* 1024 1024) ; 1MB
      kill-do-not-save-duplicates t
      history-delete-duplicates t           ; Delete duplicate history elements
      enable-recursive-minibuffers t
      read-minibuffer-restore-windows nil   ; Can mess timers which set window points
      ;; By default, to create a backup (which happens on first save), emacs renames the
      ;; file a buffer points to and then saves the buffer.  Unfortunately, this screws
      ;; with hard links.  The following solves this:
      backup-by-copying-when-linked t
      use-short-answers t)                  ; Replace yes/no prompts with y/n

(fset 'yes-or-no-p 'y-or-n-p)     ; Use y/n instead of yes/no
(when (file-exists-p "/var/tmp")
  (setq lock-file-name-transforms
        '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t))))  ; Make Emacs write all the lock files to /var/tmp/
(require 'holidays)
(setq calendar-holidays holiday-christian-holidays)

(assoc-delete-all "\\.dir-locals\\(?:-2\\)?\\.el\\'" auto-mode-alist #'string=)
(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))

(require 'savehist)
(add-to-list 'savehist-additional-variables 'kill-ring)
(add-to-list 'savehist-additional-variables 'query-replace-history)

;;; Save special buffers
;; See:

(defun lw-create-ielm-buffer (_file-name buffer-name misc)
  (let ((default-directory misc))
    ;; create a shell buffer named BUFFER-NAME in directory MISC
    (ielm)))

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

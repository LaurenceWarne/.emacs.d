(global-subword-mode 1)    ;; Moves cursor inbetween camelCase words
(global-linum-mode 1)      ;; Show line numbers
(delete-selection-mode 1)  ;; Make typing delete/overwrites selected text
(show-paren-mode 1)        ;; Turn on bracket match highlight
(desktop-save-mode 1)      ;; Save and automatically load previous emacs session on start up
(electric-pair-mode 1)     ;; Have that nice brace/parentheses auto indent
(set-face-attribute 'default nil :height 105)
(scroll-bar-mode -1)       ;; No scrollbar
(tool-bar-mode -1)         ;; Get rid of the tool bar
(menu-bar-mode -1)         ;; Get rid of the menu bar

(setq visible-bell 1)                  ;; Get rid of annoying bell, use flash screen instead
(setq show-paren-style 'parenthesis)   ;; Highlight text between parens
(setq org-use-speed-commands t)        ;; Shortcut for org commands when on headlines
(setq python-shell-interpreter "/usr/bin/python3")


(defun eclipse-indent-setup ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0))
(add-hook 'java-mode-hook 'eclipse-indent-setup)

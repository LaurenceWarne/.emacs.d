;;; init.el -- my init file
;;; Commentary:

;; See:
;; https://github.com/baron42bba/.emacs.d/blob/master/bba.org
;; https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org

;;; Code:


;; emacs 25 makes me add this
;(package-initialize)

; I split up emacs customization & initialization into smaller thematic files
(defvar custom-init-files-directory
  (concat user-emacs-directory "lisp/")
  "Directory which contains files loaded on initiliazation by init.el.")

(add-to-list 'load-path custom-init-files-directory)
; Set custom file, prevent init.el file pollution.
(setq custom-file (concat custom-init-files-directory "custom.el"))

;; Load the seperate files
(condition-case e
    (load "global-minor-mode-init")
  (error (message "error on loading global-minor-mode-init: %s"
		  (error-message-string e))))

(condition-case e
    (load "custom-functions")
  (error (message "error on loading custom-functions: %s"
		  (error-message-string e))))

(condition-case e
    (load "package-init")
  (error (message "error on loading package-init: %s"
		  (error-message-string e))))

(condition-case e
    (load "keybindings")
  (error (message "error on loading keybindings: %s"
		  (error-message-string e))))

(condition-case e
    (load "custom")
  (error (message "error on loading custom: %s"
		  (error-message-string e))))

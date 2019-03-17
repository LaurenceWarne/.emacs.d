;;; init.el -- my init file
;;; Commentary:

;; See:
;; https://github.com/baron42bba/.emacs.d/blob/master/bba.org
;; https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org

;;; Code:


;; emacs 25 makes me add this
;(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
;Set custom file, prevent init.el file pollution. 
(setq custom-file "lisp/custom.el")

;; Split into separate files for maintenence
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

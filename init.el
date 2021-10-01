;;;; init.el -- my init file
;;; Commentary:

;; For inspiration see:
;; https://github.com/baron42bba/.emacs.d/blob/master/bba.org
;; https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org

;;;; Code:

;; Split up emacs customization & initialization into smaller thematic files
(defvar lw-custom-init-files-directory
  (concat user-emacs-directory "lisp/")
  "Directory which contains custom user elisp files loaded by init.el.")

(add-to-list 'load-path lw-custom-init-files-directory)
;; Set custom file, prevent init.el file pollution.
(setq custom-file (concat lw-custom-init-files-directory "custom.el"))

(when (< (string-to-number emacs-version) 27)
  (message "An Emacs version < 27 is in use, some things may not work!"))

;;; File loading

(condition-case e
    (load "global-minor-mode-init")
  (error "Error on loading global-minor-mode-init: %s"
	 (error-message-string e)))

(condition-case e
    (load "custom-functions")
  (error "Error on loading custom-functions: %s"
	 (error-message-string e)))

(condition-case e
    (load "package-init")
  (error "Error on loading package-init: %s"
	 (error-message-string e)))

(condition-case e
    (load "keybindings")
  (error "Error on loading keybindings: %s"
	 (error-message-string e)))

(condition-case e
    (load "custom")
  (error "Error on loading custom: %s"
	 (error-message-string e)))

(condition-case e
    (when (file-exists-p (concat user-emacs-directory "/" "local"))
      (message "Found a local initialisation file")
      (load "local"))
  (error "Error on loading local: %s"
	 (error-message-string e)))

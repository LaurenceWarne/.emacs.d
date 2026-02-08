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

(when (< (string-to-number emacs-version) 29)
  (message "An Emacs version < 29 is in use, some things may not work!"))

;;; File loading
(load "custom-functions")

(load "global-minor-mode-init")

(load "vanilla-mode-init")

(load "package-init")

(load "keybindings")

(load "custom")

(when (file-exists-p (expand-file-name "local.el" lw-custom-init-files-directory))
  (message "Found a local initialisation file")
  (load "local"))

;;; init.el -- my init file
;;; Commentary:

;; See:
;; https://github.com/baron42bba/.emacs.d/blob/master/bba.org

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(load "global-minor-mode-init")
(load "custom-functions")
(load "package-init")
(load "bindings")
(load "custom")

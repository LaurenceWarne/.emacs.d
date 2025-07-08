;;;; init.el -- my init file
;;; Commentary:

;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

;;;; Code:

;; The following is suggested by https://www.reddit.com/r/emacs/comments/1j0m18u/emacs_flashing_white_at_startup/
;; but it doesn't wholely work for me
(setq default-frame-alist '((background-color . "#000000")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

;;;; custom.el -- emacs custom file
;;; Commentary:

;; 

;;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#0D0E16" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(package-selected-packages
   '(delight diminish benchmark-init mw-thesaurus try terraform-mode lua-mode ox-reveal org-reveal saws beancount beancount-mode typescript-mode typescript pony-mode lsp-cfn ialign undo-fu-session lcr org-modern zoom-window yatemplate yaml-mode yaml xr whole-line-or-region which-key undohist undo-hl ts string-inflection steam speed-type solaire-mode snow smartparens slime-company shackle sage-shell-mode rainbow-delimiters quelpa-use-package python-pytest python-isort python-black prefab poly-markdown plantuml-mode package-lint ox-yaow ox-gfm org2jekyll org-fragtog org-contrib org-bullets openapi-yaml-mode mc-biome-viewer magit-todos lsp-ui lsp-python-ms lsp-metals lsp-haskell lorem-ipsum jdoc-jumper java-snippets htmlize helpful helm-projectile helm-lsp helm-flx helm-descbinds helm-ag haskell-tng-mode groovy-mode graphviz-dot-mode graphql-mode goto-line-preview goto-chg git-modes fzf fontmenu fontify-face font-lock-studio flymake-shellcheck flycheck finito eyebrowse expand-region exec-path-from-shell esup eshell-prompt-extras eros elispfl eldev edbi doom-themes docstr dockerfile-mode docker dired-rainbow dired-filter diff-hl company-box company-auctex command-log-mode camcorder buttercup blacken beacon ascii-table all-the-icons-dired aggressive-indent adoc-mode))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "tox -e py39")
     (checkdoc-package-keywords-flag)
     (python-pytest-executable . "/home/laurencewarne/projects/cfn-lsp-extra/.nox/tests-3-9/bin/pytest")
     (lsp-python-ms-python-executable . "/home/laurencewarne/projects/cfn-lsp-extra/.nox/tests-3-9/bin/python"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(epe-pipeline-delimiter-face ((t :foreground "light green")))
 '(epe-pipeline-host-face ((t :foreground "lawn green")))
 '(epe-pipeline-user-face ((t :foreground "aquamarine" :weight bold))))

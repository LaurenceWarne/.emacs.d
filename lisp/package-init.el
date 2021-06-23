;;;; package-init.el -- installs/initializes packages -*- lexical-binding: t -*-
;;; Commentary:

;; Use use-package to install and configure packages in a readable way.
;; See:
;; https://github.com/jwiegley/use-package
;; https://jwiegley.github.io/use-package/keywords/

;;;; Code:

;;; Add repositories

;; Add melpa repository
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)  ; Get most recent versions of org mode

(package-initialize)

;; Check if use-package is installed and install if it's not
;; Note it's a melpa package so this has to come after the melpa repository is added
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

;; lsp recommends we do this for some reason
(require 'cc-mode)


;;; Use package declarations

;; Install all packages if not already installed (use-package must still be called)
(setq use-package-always-ensure t)

;; https://github.com/quelpa/quelpa
(use-package quelpa
  :config
  ;; https://github.com/quelpa/quelpa-use-package
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"))
  (require 'quelpa-use-package))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t ; if nil, italics is universally disabled

        ;; doom-one specific settings
        doom-one-brighter-modeline nil
        doom-one-brighter-comments nil)
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-acario-dark t)
  (doom-themes-org-config))

;; To ensure we have the most up to date version of org mode (at least on first
;; install) we place this use-package call before requiring org anywhere.
;; https://orgmode.org/
(use-package org
  :ensure org-plus-contrib
  :bind (:map org-mode-map
              ("C-," . beginning-of-buffer)
              ("M-n" . outline-next-heading)
              ("M-p" . outline-previous-heading)
              ("M-[" . org-backward-heading-same-level)
              ("M-]" . org-forward-heading-same-level)
              ("M-h" . (lambda () (interactive) (org-latex-preview '(16))))
              ("C-)" . nil)
              ("C-)" . nil)
              ("C-#" . nil))
  :config
  (setq
   ;; Shortcut for org commands when on headlines
   org-use-speed-commands t
   org-startup-with-inline-images t
   org-startup-folded nil
   ;; Default to normal Emacs line wrapping behaviour
   org-startup-truncated nil
   org-startup-indented t
   ;; Default of 2 is super annoying with `org-src-tab-acts-natively'
   org-edit-src-content-indentation 0
   org-feed-alist
   '(("Org"
      "https://blog.tecosaur.com/tmio/rss.xml"
      "~/org/feeds.org"
      "Weekly Org Entries"))
   org-confirm-babel-evaluate nil)
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (emacs-lisp . t)))
  ;; Horrfic hack to disable highlight-indent-mode in python snippets
  ;; which are exported to html using org export.
  ;; See the defintion of `org-html-fontify-code' for why this works
  (defun python-no-elpy-mode ()
    (interactive)
    (let (python-mode-hook)
      (python-mode)))
  (add-to-list 'org-src-lang-modes '("python" . python-no-elpy))
  ;; End hack
  :pin org)

;; https://github.com/Alexander-Miller/pfuture
(use-package pfuture)

;; https://github.com/magnars/dash.el
(use-package dash
  :config
  (global-dash-fontify-mode))

(use-package f)

(use-package ace-window
  :config
  (bind-key "M-o" 'ace-window))

;; https://github.com/jorgenschaefer/elpy
(use-package elpy
  :bind (:map python-mode-map
              ("C-x C-e" . #'elpy-shell-send-statement))
  ;; Enable Elpy in all future Python buffers.
  :init (elpy-enable)
  :config (setq elpy-rpc-python-command "python3")
  ;; Fix python does not support readline warning
  (setq python-shell-completion-native-enable nil))

(use-package avy
  ;; This does two things: first, it creates an autoload for the avy-goto-char commands and defers loading of avy until you actually use it. Second, it binds the key C-: to that command.
  :bind
  ("C-:" . avy-goto-char)
  ("C-;" . avy-goto-char-timer)
  ("C-#" . avy-copy-region)
  ("M-#" . avy-copy-line)
  :config
  (setq avy-keys-alist
        `((avy-goto-char-2 . (?a ?s ?d ?f ?j ?k ?l)))))

(use-package yasnippet
  :defer t
  :init
  (autoload 'yasnippet "yasnippet" nil t)
  :config
  (yas-global-mode 1)
  (setq yas-indent-line 'auto
        yas-also-auto-indent-first-line t))

(use-package smartparens
  :demand t
  :bind (:map smartparens-mode-map
              ("C-0" . sp-forward-slurp-sexp)
              ("C-9" . sp-forward-barf-sexp)
              ("C--" . sp-unwrap-sexp)
              ("C-r" . sp-up-sexp)
              ("M-r" . sp-backward-up-sexp)
              ("M-f" . sp-forward-sexp)
              ("M-b" . sp-backward-sexp)
              ("M-s" . sp-down-sexp)
              ("M-t" . sp-transpose-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-escape-quotes-after-insert t)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'slime-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'ielm-mode-hook #'smartparens-strict-mode)
  (smartparens-global-mode 1))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure nil
  :quelpa (projectile :fetcher github :repo "bbatsov/projectile" :upgrade t)
  ;:load-path "~/projects/projectile"
  :demand t
  :bind
  ("M-p" . projectile-switch-project)
  ("C-c C-c" . projectile-test-project)
  ("C-c C-r" . projectile-run-project)
  :config
  (projectile-mode 1)
  (setq projectile-create-missing-test-files t
        projectile-other-file-alist
        (append projectile-other-file-alist
                '(("md"    . ("el" "java" "py" "scala" "yml" "yaml" "ini" "gradle"))
                  ("ini"   . ("el" "java" "py" "scala" "yml" "yaml" "md" "gradle"))
                  ("yml"   . ("el" "java" "py" "scala" "ini" "md" "gradle" "yml" "yaml"))
                  ("yaml"  . ("el" "java" "py" "scala" "ini" "md" "gradle" "yml" "yaml"))
                  ("conf"  . ("el" "java" "py" "scala" "ini" "md" "gradle" "yml" "yaml"))
                  ("el"    . ("el" "md" "org"))
                  ("py"    . ("py" "md" "ini" "yml" "yaml"))
                  ("java"  . ("java" "md" "gradle" "yml" "yaml"))
                  ("scala" . ("scala" "sc" "md" "gradle" "yml" "yaml" "jenkinsfile" "org" "tf"))
                  ("sc"    . ("scala" "sc" "md" "gradle" "yml" "yaml" "conf"))
                  ("sbt"   . ("scala" "sbt" "md" "gradle" "yml" "yaml" "conf"))
                  ("org"   . ("org"))))
        lw-sbt-related-files
        (list
         (projectile-related-files-fn-test-with-suffix "scala" "Test")
         (projectile-related-files-fn-test-with-suffix "scala" "Tests")
         (projectile-related-files-fn-test-with-suffix "scala" "Suite")
         (projectile-related-files-fn-test-with-suffix "scala" "Spec"))
        lw-eldev-related-files
        (list
         (projectile-related-files-fn-test-with-suffix "el" "-test")
         (projectile-related-files-fn-test-with-prefix "el" "test-"))
        ;; Since bloop takes priority over sbt (.bloop file)
        projectile-project-types
        (--remove (eq (car it) 'bloop) projectile-project-types))
  (defun lw-projectile-run-test-file ()
    "Run a the test file in the current buffer, as opposed to all tests."
    (interactive)
    (when-let (test-file-fn
               (projectile-project-type-attribute
                (projectile-project-type) 'test-file-fn))
      (unless (projectile-test-file-p (buffer-file-name))
        (save-current-buffer
          (projectile-toggle-between-implementation-and-test)
          (funcall test-file-fn)))
      (funcall test-file-fn)))
  (cl-defun lw-projectile-update-project-type-override (old-fn project-type &key marker-files project-file compilation-dir configure compile install package test run test-suffix test-prefix src-dir test-dir related-files-fn test-file-fn)
    (funcall old-fn project-type
             :marker-files marker-files
             :project-file project-file
             :compilation-dir compilation-dir
             :configure configure
             :compile compile
             :install install
             :package package
             :test test
             :run run
             :test-suffix test-suffix
             :test-prefix test-prefix
             :src-dir src-dir
             :test-dir test-dir
             :related-files-fn related-files-fn)
    (setq projectile-project-types
          (--map-when (and test-file-fn (eq (car it) project-type))
                      (append it (list 'test-file-fn test-file-fn))
                      projectile-project-types)))
  (advice-add 'projectile-update-project-type
              :around
              #'lw-projectile-update-project-type-override)
  (defun lw-projectile-test-file ()
    (interactive)
    (when-let* ((test-file-fn
                 (projectile-project-type-attribute
                  (projectile-project-type) 'test-file-fn))
                (current-file (buffer-file-name))
                (target-file (if (projectile-test-file-p current-file) current-file (projectile-find-implementation-or-test current-file)))
                (command-str (funcall test-file-fn target-file)))
      (projectile--run-project-cmd command-str
                                   (make-hash-table)
                                   :show-prompt 0
                                   :prompt-prefix "Test command: "
                                   :save-buffers t)))
  (define-key projectile-mode-map (kbd "C-c C-f") #'lw-projectile-test-file)
  (defun lw-sbt-test-file-fn (file-name)
    (interactive)
    (concat (lw-sbt-command) " 'testOnly "
            (lw-jvm-get-file-package (f-dirname file-name))
            "." (f-no-ext (f-filename file-name)) "'"))
  (defun lw-sbt-command ()
    (if (locate-file "sbtn" exec-path) "sbtn" "sbt"))
  (defalias 'lw-sbt-compile-cmd (lambda () (concat (lw-sbt-command) " compile")))
  (defalias 'lw-sbt-test-cmd (lambda () (concat (lw-sbt-command) " test")))
  (defalias 'lw-sbt-run-cmd (lambda () (concat (lw-sbt-command) " run")))
  (defun dir-swap (str replacement)
    (lambda (file-path) (projectile-complementary-dir file-path str replacement)))
  (projectile-update-project-type
   'sbt
   :compile #'lw-sbt-compile-cmd
   :test  #'lw-sbt-test-cmd
   ;; Only for projectile-create-missing-test-files
   :test-suffix "Test"
   :run #'lw-sbt-run-cmd
   :src-dir (dir-swap "test" "main")
   :test-dir (dir-swap "main" "test")
   :related-files-fn lw-sbt-related-files
   :test-file-fn #'lw-sbt-test-file-fn)
  (projectile-update-project-type
   'mill
   :src-dir (dir-swap "test/src" "src")
   :test-dir (dir-swap "src" "test/src"))
  (projectile-update-project-type
   'maven
   :src-dir (dir-swap "test" "main")
   :test-dir (dir-swap "main" "test"))
  (projectile-update-project-type
   'gradlew
   :test-suffix
   "Test"
   :src-dir (dir-swap "test" "main")
   :test-dir (dir-swap "main" "test"))
  (projectile-update-project-type
   'emacs-eldev
   :related-files-fn lw-eldev-related-files))

;; http://tuhdo.github.io/helm-intro.html
(use-package helm
  :demand t
  :bind
  ("M-y" . helm-show-kill-ring)
  ("C-x C-f" . helm-find-files)
  ("C-j" . helm-mini)
  ("C-x b" . helm-buffers-list)
  ("M-x" . helm-M-x)
  ("C-s" . helm-occur)
  :config
  (helm-mode 1)
  ;; Makes helm-boring-file-regexp-list act as a .gitignore
  (setq helm-ff-skip-boring-files t
        helm-M-x-fuzzy-match t
        helm-split-window-in-side-p t)
  (define-key helm-map (kbd "C-,") 'helm-beginning-of-buffer)
  (define-key helm-map (kbd "C-.") 'helm-end-of-buffer)
  (define-key helm-map (kbd "C-k") 'helm-buffer-run-kill-buffers)
  (define-key helm-map (kbd "M-D") 'helm-delete-minibuffer-contents)
  (define-key helm-occur-map (kbd "C-s") 'helm-next-line)
  (define-key helm-occur-map (kbd "C-r") 'helm-previous-line)
  (require 'org)
  (define-key org-mode-map (kbd "C-j") 'helm-mini))

(use-package helm-flx
  :after helm
  :config
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t ; t by default
        helm-flx-for-helm-locate t))   ; nil by default

(use-package helm-ag
  :after helm
  :bind (("C-q" . helm-do-ag)))

(use-package helm-projectile
  ;; Don't add helm-ag to after because its loading is deferred
  :after (helm projectile)
  :init
  (require 'markdown-mode)
  :bind (:map python-mode-map
              ("C-j" . #'helm-projectile)
              ("M-q" . #'helm-projectile-ag)
              ("M-k" . #'projectile-toggle-between-implementation-and-test)
              :map java-mode-map
              ("C-j" . #'helm-projectile)
              ("M-q" . #'helm-projectile-ag)
              ("M-k" . #'projectile-toggle-between-implementation-and-test)
              :map emacs-lisp-mode-map
              ("C-j" . #'helm-projectile)
              ("M-q" . #'helm-projectile-ag)
              ("M-k" . #'projectile-toggle-between-implementation-and-test)
              :map markdown-mode-map
              ("C-j" . #'helm-projectile)
              ("M-q" . #'helm-projectile-ag)
              :map org-mode-map
              ("C-j" . #'helm-projectile))
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  ;; Here because :config in projectile is kinda... full
  (defun lw-switch-to-last-buffer()
    "Switch to buffer returned by (other-buffer)."
    (interactive)
    (if (projectile-project-type)
        (switch-to-buffer (cadr (projectile-project-buffers)))
      ;; Check here if line is empty
      (switch-to-buffer nil)))
  (global-set-key (kbd "M-j") 'lw-switch-to-last-buffer))

(use-package helm-descbinds
  :after helm
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package company
  :config
  ;; We usually want make sure we have appropriate backends before enabling
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'ielm-mode-hook 'company-mode))

(use-package flycheck
  ;; Don't use :hook here as that defers loading until flycheck is called
  :config (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

(use-package hydra)

;; Note groovy mode automatically adds itself to auto-mode-alist
(use-package groovy-mode
  :after helm-projectile
  :bind (:map groovy-mode-map
              ("C-j" . #'helm-projectile)
              ("M-q" . #'helm-projectile-ag)
              ("M-k" . #'projectile-toggle-between-implementation-and-test)))

;; https://github.com/parkouss/speed-type
(use-package speed-type
  :quelpa (speed-type :fetcher github :repo "laurencewarne/speed-type" :upgrade t)
  :commands (speed-type-code-region speed-type-text speed-type-region speed-type-buffer)
  ;; :load-path "~/projects/speed-type"
  :config
  (setq speed-type-default-lang "English"))

(use-package goto-last-change
  :bind
  ("C-'" . goto-last-change))

(use-package java-snippets
  :after yasnippet)

(use-package lsp-mode
  :after scala-mode
  :hook
  (java-mode . lsp-deferred)
  (scala-mode . lsp-deferred)
  (lsp-mode . lsp-lens-mode)
  (scala-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t)))
  :config
  (setq lsp-keep-workspace-alive nil
        lsp-enable-file-watchers nil
        lsp-enable-links nil
        lsp-headerline-breadcrumb-enable nil)
  (when-let* ((go-dir (concat (getenv "HOME") "/go/bin/sqls"))
              ((f-exists? go-dir)))
    (setq lsp-sqls-server go-dir)))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point))

(use-package lsp-java
  :after lsp-mode
  :config
  (setq lsp-java-format-comments-enabled nil
        lsp-java-format-on-type-enabled nil
        lsp-java-save-actions-organize-imports t)
  (setq tab-width 4))

(use-package dap-mode
  :after lsp-mode)

(use-package dap-java :ensure nil)

(use-package helm-lsp
  :after lsp-mode helm
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package eyebrowse
  :config
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
  (define-key eyebrowse-mode-map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
  (define-key eyebrowse-mode-map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
  (define-key eyebrowse-mode-map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
  (define-key eyebrowse-mode-map (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

;; https://github.com/Malabarba/camcorder.el
(use-package camcorder
  :commands camcorder-mode
  :config
  (define-key camcorder-mode-map (kbd "C-<f12>") 'camcorder-stop))

;; https://github.com/purcell/package-lint
(use-package package-lint)

;; https://github.com/LaurenceWarne/jdoc-jumper
(use-package jdoc-jumper
  :ensure nil
  :quelpa (jdoc-jumper :fetcher github :repo "laurencewarne/jdoc-jumper" :upgrade t)
  ;; :load-path "~/projects/jdoc-jumper"
  :commands jdoc-jumper-jump-from-point)

;; https://github.com/politza/pdf-tools/blob/master/README.org
(use-package pdf-tools
  :config
  (pdf-loader-install)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-,") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "C-.") 'pdf-view-last-page)
  (define-key pdf-view-mode-map (kbd "C--") 'pdf-view-shrink)
  (define-key pdf-view-mode-map (kbd "C-=") 'pdf-view-enlarge)
  (set-face-attribute 'pdf-isearch-lazy nil
                      :inherit 'lazy-highlight
                      :foreground "black"
                      :background "grey")
  (set-face-attribute 'pdf-isearch-match nil
                      :inherit 'isearch
                      :foreground "white"
                      :background "black"))

;; https://github.com/anwyn/slime-company
(use-package slime-company
  :after company
  ;; We have to call this before slime is loaded:
  ;; https://github.com/anwyn/slime-company/issues/11
  :init
  (slime-setup '(slime-fancy slime-company)))

(use-package slime
  :config
  ;; Set your lisp system and, optionally, some contribs
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
        slime-contribs '(slime-fancy))
  (define-key slime-repl-mode-map (kbd "M-,") 'slime-describe-symbol)
  (define-key slime-repl-mode-map (kbd "C-c C-d C-d") 'slime-pop-find-definition-stack))

(use-package steam
  ;; :load-path "~/projects/steam.el"
  :quelpa (steam :fetcher github :repo "laurencewarne/steam.el" :upgrade t)
  :config
  (setq steam-username "39422361280608732623190235"))

(use-package beacon
  :config
  (beacon-mode 1))

;; Requires shellcheck:
;; https://github.com/koalaman/shellcheck
;; https://github.com/federicotdn/flymake-shellcheck
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

;; The emacs startup profiler
;; https://github.com/jschaf/esup
(use-package esup
  :commands esup)

;; https://github.com/jschaf/emacs-lorem-ipsum
(use-package lorem-ipsum
  :commands
  (lorem-ipsum-insert-sentences
   lorem-ipsum-insert-paragraphs
   lorem-ipsum-insert-list))

;; https://github.com/millejoh/emacs-ipython-notebook
(use-package ein)

;; https://github.com/alphapapa/org-rifle
(use-package helm-org-rifle
  :config
  (require 'org)
  (define-key org-mode-map (kbd "M-r") #'helm-org-rifle-org-directory))

;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key))

;; https://magit.vc/
(use-package magit
  :bind
  ("C-x g" . #'magit)
  ("C-c g" . #'magit-file-dispatch)
  :config
  (defun lw-magit-checkout-last (&optional start-point)
    (interactive)
    (magit-branch-checkout "-" start-point))
  (transient-append-suffix 'magit-branch "w"
    '("-" "last branch" lw-magit-checkout-last)))

;; https://github.com/syohex/emacs-zoom-window
(use-package zoom-window
  :config
  (global-set-key (kbd "M-i") 'zoom-window-zoom))

;; https://github.com/sebastiencs/company-box
;; Need to M-x install-all-the-icons
(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :config
  (global-set-key (kbd "M-'") 'er/expand-region))

(use-package magit-todos
  :config
  (magit-todos-mode))

(use-package mc-biome-viewer
  :ensure nil
  ;; :load-path "~/projects/mc-biome-viewer"
  :quelpa (mc-biome-viewer :fetcher github :repo "LaurenceWarne/mc-biome-viewer" :upgrade t)
  ;; Example configuration
  :config
  (setq mc-biome-viewer-column-chunks-in-camera 48)  ; But fewer chunks will be faster
  (puthash "ice plains" '(:foreground "silver") mc-biome-viewer-biome-to-face-map))

;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; https://github.com/ardumont/org2jekyll
(use-package org2jekyll
  :quelpa (org2jekyll :fetcher github :repo "laurencewarne/org2jekyll" :upgrade t)
  ;; :load-path "~/projects/org2jekyll"
  :config
  (setq org2jekyll-blog-author "Laurence Warne"
        org2jekyll-source-directory (expand-file-name "~/posts/")
        org2jekyll-jekyll-directory (expand-file-name "~/Documents/")
        org2jekyll-jekyll-drafts-dir ""
        org2jekyll-jekyll-posts-dir "_posts/"
        post-project
        `("post"
          :base-directory ,(org2jekyll-input-directory)
          :base-extension "org"
          :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
          :publishing-function org-html-publish-to-html
          :headline-levels 4
          :section-numbers nil
          :with-toc nil
          :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
          :html-preamble t
          :recursive t
          :make-index t
          :html-extension "html"
          :body-only t))
  (setq org-publish-project-alist (cons post-project org-publish-project-alist)))

;; https://github.com/hniksic/emacs-htmlize
(use-package htmlize)

;; https://github.com/domtronn/all-the-icons.el
;; Note after installing this you need to run M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; https://github.com/Fuco1/dired-hacks
(use-package dired-filter
  :hook (dired-mode . dired-filter-group-mode)
  :config
  (setq dired-filter-group-saved-groups
        '(("default"
           ("Git"
            (regexp . "^\\.git"))
           ("Python"
            (extension . "py"))
           ("Java"
            (extension . "java"))
           ("Lisp"
            (extension "el" "cl" "elc"))
           ("Org"
            (extension . "org"))
           ("PDF"
            (extension . "pdf"))
           ("LaTeX"
            (extension "tex" "bib"))
           ("HTML"
            (extension "html"))
           ("Archives"
            (extension "zip" "rar" "gz" "bz2" "tar"))
           ("Images"
            (extension "jpg" "png" "jpeg" "gif" "bmp" "svg"))
           ("Media"
            (extension "mp3" "mp4" "avi" "mpg" "flv" "ogg"))
           ("Configuration"
            (regexp . "^\\.[^(git)]+"))
           ("Backup"
            (regexp . ".*~"))))))

;; https://github.com/Fuco1/dired-hacks#dired-rainbow
(use-package dired-rainbow
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package pcre2el)

(use-package ox-yaow
  :after org
  :ensure nil
  :quelpa (ox-yaow :fetcher github :repo "laurencewarne/ox-yaow.el" :upgrade t)
  ;; :load-path "~/projects/ox-yaow.el"
  :config
  ;; Stolen from https://github.com/fniessen/org-html-themes
  (setq rto-css '("https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/htmlize.css"
                  "https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/readtheorg.css")
        rto-js '("https://fniessen.github.io/org-html-themes/src/lib/js/jquery.stickytableheaders.min.js"
                 "https://fniessen.github.io/org-html-themes/src/readtheorg_theme/js/readtheorg.js")
        extra-js '("https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"
                   "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js" )
        ox-yaow-html-head (concat (mapconcat (lambda (url) (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" url "\"/>\n")) rto-css "")
                                  (mapconcat (lambda (url) (concat "<script src=\"" url "\"></script>\n")) (append rto-js extra-js) ""))
        org-publish-project-alist (cons
                                   `("wiki"
                                     :base-directory "~/org/"
                                     :base-extension "org"
                                     :publishing-directory "~/wiki/"
                                     :html-head ,ox-yaow-html-head
                                     :html-preamble t
                                     :recursive t
                                     :exlude ".*steam\.org"
                                     :publishing-function ox-yaow-publish-to-html
                                     :preparation-function ox-yaow-preparation-fn
                                     :completion-function ox-yaow-completion-fn
                                     :ox-yaow-file-blacklist ("~/org/maths/answers.org")
                                     :ox-yaow-depth 2)
                                   org-publish-project-alist)))

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode)

;; https://github.com/lewang/command-log-mode
(use-package command-log-mode)

;; https://github.com/sagemath/sage-shell-mode
(use-package sage-shell-mode)

;; https://github.com/Fuco1/fontify-face
(use-package fontify-face
  :hook (emacs-lisp-mode . fontify-face-mode))

;; https://github.com/cireu/elispfl
(use-package elispfl
  :ensure nil
  :quelpa (elispfl :fetcher github :repo "cireu/elispfl")
  :hook ((emacs-lisp-mode . elispfl-mode)
         (ielm-mode . elispfl-ielm-mode)))

;; https://github.com/kjambunathan/fontmenu
(use-package fontmenu
  :quelpa (fontmenu :fetcher github :repo "laurencewarne/fontmenu" :upgrade t)
  :load-path "~/projects/fontmenu")

;; https://github.com/mineo/yatemplate
(use-package yatemplate
  :after yasnippet
  :config
  (auto-insert-mode)
  (setq auto-insert-alist nil  ; is already populated by default
        auto-insert-query nil)
  (yatemplate-fill-alist))

;; https://github.com/io12/org-fragtog
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; https://github.com/alphapapa/snow.el
(use-package snow
  :ensure nil
  :quelpa (snow :fetcher github :repo "alphapapa/snow.el")
  :commands let-it-snow)

;; https://github.com/skuro/plantuml-mode
;; Also see:
;; https://plantuml.com/class-diagram
(use-package plantuml-mode
  :after org
  :config
  (require 'org)
  (require 'ob-plantuml)
  (setq plantuml-jar-path (concat user-emacs-directory "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)
  (unless (file-exists-p plantuml-jar-path)
    (plantuml-download-jar))
  (plantuml-set-exec-mode "jar")
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; https://github.com/wbolster/emacs-python-black
(use-package python-black)

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :config
  (which-key-mode))

;; Also requires:
;; pip3 install isort --user
;; https://github.com/paetzke/py-isort.el
(use-package py-isort
  :hook (before-save . py-isort-before-save))

;; Also requires:
;; pip3 install importmagic epc --user
;; https://github.com/anachronic/importmagic.el
                                        ;(use-package importmagic
                                        ;    :ensure t
                                        ;    :config
                                        ;    (add-hook 'python-mode-hook 'importmagic-mode))

;; https://github.com/hvesalai/emacs-scala-mode
(use-package scala-mode
  :after f smartparens projectile
  :mode "\\.s\\(c\\|cala\\|bt\\)$"
  :bind (:map scala-mode-map
              ("C-j" . #'helm-projectile)
              ("M-q" . #'helm-projectile-ag)
              ("M-k" . #'projectile-toggle-between-implementation-and-test))
  :config
  ;; :shake-fist: https://github.com/sdkman/sdkman-cli/issues/568
  (let ((sdkman-dir "~/.sdkman/candidates/"))
    (when (f-directory-p sdkman-dir)
      (-each (f-directories
              (f-expand sdkman-dir)
              (lambda (dir) (equal '("current" "bin") (last (f-split dir) 2)))
              t)
        (lambda (path) (setenv "PATH" (concat path ":" (getenv "PATH"))))))))

;; https://scalameta.org/metals/docs/editors/emacs.html
;; Note this package requires installation of a binary (see above link)
(use-package lsp-metals
  :after lsp lsp-ui scala-mode)

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; https://github.com/ppareit/graphviz-dot-mode
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

;; https://depp.brause.cc/shackle
(use-package shackle
  :config
  (cl-defun lw-shackle-get-window (buffer alist plist &optional (other-window t))
    (save-selected-window
      (if-let ((existing-window (get-buffer-window buffer)))
          existing-window
        (when other-window (other-window 1))
        (let ((win (split-window-below)))
          (select-window win)
          (switch-to-buffer buffer)
          (local-set-key (kbd "q") 'kill-buffer-and-window)  ; won't work in interactive buffers
          win))))
  (defalias 'lw-shackle-get-window-cur
    (lambda (buffer alist plist)
      (lw-shackle-get-window buffer alist plist nil)))

  (setq shackle-rules '((compilation-mode :select nil :custom lw-shackle-get-window)
                        ("magit: .*" :regexp t :select t :custom lw-shackle-get-window-cur)
                                        ;("\*docker.*" :regexp t :select t :custom lw-shackle-get-window-cur)
                        ))
  (shackle-mode 1))

(use-package company-graphviz-dot
  :disabled
  :after company graphviz-dot-mode)

;; https://github.com/dalanicolai/pdf-continuous-scroll-mode.el
(use-package pdf-continuous-scroll-mode
  :ensure nil
  :quelpa (pdf-continuous-scroll-mode :fetcher github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :hook (pdf-view-mode . pdf-continuous-scroll-mode))

;; https://github.com/manateelazycat/emacs-application-framework#dependency-list
;; Note extra installation steps are required, see above link
(use-package eaf
  :if (and (eq system-type 'gnu/linux) (getenv "DBUS_SESSION_BUS_ADDRESS"))
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (setq eaf-python-command (concat user-emacs-directory "eaf/venv/bin/python3"))
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding))

(use-package openapi-yaml-mode
  :after eaf
  :ensure nil
  :quelpa (openapi-yaml-mode :fetcher github :repo "magoyette/openapi-yaml-mode")
  :config
  (setq lw-openapi-jar-path "~/Downloads/swagger-codegen-cli.jar"
        lw-openapi-output-dir "out/openapi")
  (defun lw-openapi-to-html ()
    (interactive)
    (shell-command
     (concat
      "java -jar "
      lw-openapi-jar-path
      " "
      (buffer-file-name)
      " -l html2 -o " lw-openapi-output-dir))
    (let ((dir default-directory))
      (other-window 1)
      (eaf-open (concat dir lw-openapi-output-dir "/index.html"))))
  (define-key openapi-yaml-mode-map (kbd "C-c C-c") #'lw-openapi-to-html))

;; https://github.com/larstvei/ox-gfm
(use-package ox-gfm)

;; https://github.com/kiwanami/emacs-edbi
(use-package edbi)

;; https://github.com/Silex/docker.el
(use-package docker
  :ensure nil
  :quelpa (docker :fetcher github :repo "Silex/docker.el" :upgrade t)
  :bind (("C-c d" . docker)
         :map docker-container-mode-map
         ("q" . kill-buffer-and-window)
         :map docker-image-mode-map
         ("q" . kill-buffer-and-window)
         :map docker-network-mode-map
         ("q" . kill-buffer-and-window)
         :map docker-volume-mode-map
         ("q" . kill-buffer-and-window)
         :map docker-machine-mode-map
         ("q" . kill-buffer-and-window))
  :config
  (add-to-list
   'docker-image-run-custom-args
   `("^postgres" ("-e POSTGRES_PASSWORD=postgres" . ,docker-run-default-args))))

;; https://github.com/jcs-elpa/goto-line-preview
(use-package goto-line-preview
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

;; https://github.com/jcs-elpa/company-fuzzy
(use-package company-fuzzy
  :disabled  ; Too slow!
  :config
  (global-company-fuzzy-mode 1))

;; https://github.com/jcs-elpa/docstr
(use-package docstr
  :quelpa (docstr :fetcher github :repo "laurencewarne/docstr" :upgrade t)
  :after scala-mode
  :hook
  (scala-mode . docstr-mode)
  (java-mode . docstr-mode)
  (python-mode . docstr-mode)
  :config
  (setq docstr-key-support t))

;; https://github.com/jorgenschaefer/emacs-buttercup
;; We install this package to get the correct indentation for `describe' and
;; `it' blocks when writing tests.
(use-package buttercup)

;; https://gitlab.com/pidu/git-timemachine
(use-package git-timemachine)

;; https://github.com/LaurenceWarne/finito.el
(use-package finito
  :ensure nil  
  :quelpa (finito :fetcher github :repo "laurencewarne/finito.el" :upgrade t))

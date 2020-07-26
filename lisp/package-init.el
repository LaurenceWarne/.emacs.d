;;;; package-init.el -- installs/initializes packages
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

;; https://orgmode.org/
(use-package org
  :ensure org-plus-contrib
  :bind (:map org-mode-map
              ("C-," . 'beginning-of-buffer)
              ("M-n" . 'outline-next-heading)
              ("M-p" . 'outline-previous-heading)
              ("M-[" . 'org-backward-heading-same-level)
              ("M-]" . 'org-forward-heading-same-level)
              ("M-h" . (lambda () (interactive) (org-toggle-latex-fragment 16))))
  :config
  (setq org-use-speed-commands t     ; Shortcut for org commands when on headlines
        org-startup-with-inline-images t
        org-startup-folded nil
        org-startup-truncated nil)   ; Default to normal Emacs line wrapping behaviour
  :pin org)

(use-package ace-window
  :config
  (bind-key "M-o" 'ace-window))

;; NOTE:
;; for this package to work you need to run:
;; $ pip3 install jedi flake8 autopep8 black yapf --user
;; See:
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
  ("C-:" . 'avy-goto-char)
  ("C-;" . 'avy-goto-char-timer)
  ("C-#" . 'avy-copy-region)
  ("M-#" . 'avy-copy-line)
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
	      ;; We use smartparens as a replacement for paredit in python buffers
	      ("C-0" . 'sp-forward-slurp-sexp)
	      ("C-9" . 'sp-forward-barf-sexp))
  :config
  (add-hook 'inferior-python-mode-hook #'smartparens-mode)
  (add-hook 'java-mode-hook #'smartparens-mode)
  (add-hook 'python-mode-hook #'smartparens-mode))
  ;(smartparens-global-mode 1))

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-other-file-alist
	(append projectile-other-file-alist
		'(("el"   . ("el" "md"))
		  ("py"   . ("py" "md"))
		  ("java" . ("java" "md" "gradle"))))))

;; http://tuhdo.github.io/helm-intro.html
(use-package helm
  :demand t
  :bind
  ("M-y" . 'helm-show-kill-ring)
  ("C-x C-f" . 'helm-find-files)
  ("C-j" . 'helm-mini)
  ("C-x b" . 'helm-buffers-list)
  ("M-x" . 'helm-M-x)
  ("C-s" . 'helm-occur)
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
  :after (helm projectile groovy-mode paredit)
  :init
  (defun lw-helm-projectile-find-other-file-pf ()
    (interactive)
    (let ((current-prefix-arg 4))
      (call-interactively #'helm-projectile-find-other-file)))
  :bind (:map python-mode-map
	      ("C-j" . #'helm-projectile)
	      ("M-q" . #'helm-projectile-ag)
	      ("M-k" . #'lw-helm-projectile-find-other-file-pf)
	      :map java-mode-map
	      ("C-j" . #'helm-projectile)
	      ("M-q" . #'helm-projectile-ag)
	      ("M-k" . #'lw-helm-projectile-find-other-file-pf)
	      :map emacs-lisp-mode-map
	      ("C-j" . #'helm-projectile)
	      ("M-q" . #'helm-projectile-ag)
	      ("M-k" . #'lw-helm-projectile-find-other-file-pf)
	      :map paredit-mode-map
	      ("C-j" . #'helm-projectile)
	      ("M-q" . #'helm-projectile-ag)
	      ("M-k" . #'lw-helm-projectile-find-other-file-pf)
	      :map groovy-mode-map
	      ("C-j" . #'helm-projectile)
	      ("M-q" . #'helm-projectile-ag)
	      ("M-k" . #'lw-helm-projectile-find-other-file-pf))
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm))

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
(use-package groovy-mode)

;; https://github.com/parkouss/speed-type
(use-package speed-type
  :commands (speed-type-code speed-type-text speed-type-region speed-type-buffer)
  :load-path "~/projects/speed-type")

(use-package goto-last-change
  :bind
  ("C-'" . 'goto-last-change))

(use-package java-snippets
  :after yasnippet)

(use-package lsp-mode
  :hook
  (java-mode . lsp-deferred)
  :config
  (setq lsp-keep-workspace-alive nil
	lsp-enable-file-watchers nil
	lsp-enable-links nil))

(use-package company-lsp)

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
  ;; Do we need to start the server (if not already running) here as well?
  (setq lsp-java-vmargs
	;; Needs lombok in the gradle cache
  	'("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/laurencewarne/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.10/625fc0055674dff70dbc76efa36d0f2c89b04a24/lombok-1.18.10.jar" "-Xbootclasspath/a:/home/laurencewarne/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.10/625fc0055674dff70dbc76efa36d0f2c89b04a24/lombok-1.18.10.jar"))
  (setq tab-width 4))

(use-package helm-lsp
  :after lsp-mode helm
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package doom-themes
  :hook
  ((find-file-hook after-revert-hook) . doom-buffer-mode-maybe)
  (ediff-prepare-buffer-hook . doom-buffer-mode)
  (minibuffer-setup-hook . doom-brighten-minibuffer)
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t  ; if nil, italics is universally disabled

	;; doom-one specific settings
	doom-one-brighter-modeline nil
	doom-one-brighter-comments nil)
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-one t))

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
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
  (define-key camcorder-mode-map (kbd "S-<f12>") 'camcorder-stop))

;; https://github.com/purcell/package-lint
(use-package package-lint)

;; https://github.com/LaurenceWarne/jdoc-jumper
(use-package jdoc-jumper
  :commands jdoc-jumper-jump-from-point
  :load-path "~/projects/jdoc-jumper")

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
  :load-path "~/projects/steam.el"
  :config
  (setq steam-username "39422361280608732623190235"
	org-startup-with-inline-images t))

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

(use-package lorem-ipsum
  :commands
  (lorem-ipsum-insert-sentences
   lorem-ipsum-insert-paragraphs
   lorem-ipsum-insert-list))

;; https://github.com/millejoh/emacs-ipython-notebook
(use-package ein)

;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :bind (:map paredit-mode-map
	      ("C-0" . 'paredit-forward-slurp-sexp)
	      ("C-9" . 'paredit-backward-slurp-sexp)
	      ("M-s" . 'forward-sexp))
  :config
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode))

;; https://github.com/vermiculus/sx.el
(use-package sx
  :config
  (bind-keys :prefix "C-c s"
             :prefix-map my-sx-map
             :prefix-docstring "Global keymap for SX."
             ("q" . sx-tab-all-questions)
             ("i" . sx-inbox)
             ("o" . sx-open-link)
             ("u" . sx-tab-unanswered-my-tags)
             ("a" . sx-ask)
             ("s" . sx-search)))

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
(use-package magit)

;; https://github.com/syohex/emacs-zoom-window
(use-package zoom-window
  :config
  (global-set-key (kbd "M-i") 'zoom-window-zoom))

;; https://github.com/sebastiencs/company-box
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :config
  (global-set-key (kbd "M-'") 'er/expand-region))

(use-package magit-todos)

(use-package mc-biome-viewer
    :ensure nil
    :load-path "~/projects/mc-biome-viewer"
    ;:quelpa (mc-biome-viewer :fetcher github :repo "LaurenceWarne/mc-biome-viewer")
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
    :load-path "~/projects/org2jekyll"
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

;; Requires cquery: https://github.com/cquery-project/cquery/wiki/Emacs
;; https://github.com/cquery-project/emacs-cquery
(use-package cquery
  :config
  (setq cquery-executable "/usr/local/src/cquery/build/cquery"))

(use-package ox-yaow
    :ensure nil
    :load-path "~/projects/ox-yaow.el"
    :config
    (setq org-publish-project-alist
	  (cons	'("wiki"
		 :base-directory "~/org/"
		 :base-extension "org"
		 :publishing-directory "~/wiki/"
		 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/htmlize.css\"/><link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/readtheorg.css\"/><script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script><script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/lib/js/jquery.stickytableheaders.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/js/readtheorg.js\"></script>"
		 :html-preamble t
		 :recursive t
		 :publishing-function ox-yaow-publish-to-html
		 :preparation-function ox-yaow-preparation-fn
		 :completion-function ox-yaow-completion-fn
		 :ox-yaow-depth 1)
		org-publish-project-alist)))

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode)

;; https://github.com/lewang/command-log-mode
(use-package command-log-mode)

;; https://github.com/sagemath/sage-shell-mode
(use-package sage-shell-mode)

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
  :load-path "~/projects/fontmenu")

;; https://github.com/mineo/yatemplate
(use-package yatemplate
  :config
  (auto-insert-mode)
  (setq auto-insert-alist nil  ; is already populated by default
        auto-insert-query nil)
  (yatemplate-fill-alist))

;; https://github.com/io12/org-fragtog
(use-package org-fragtog
 :hook ((org-mode . org-fragtog-mode)))

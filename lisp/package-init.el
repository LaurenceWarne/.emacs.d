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
  
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

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

;; https://github.com/magnars/dash.el
(use-package dash
  :demand t
  :config
  (global-dash-fontify-mode))

;; https://github.com/rejeep/f.el
(use-package f
  :demand t)

;; https://github.com/magnars/s.el
(use-package s
  :demand t)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(x mac ns))
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
  :after dash
  :bind (:map org-mode-map
              ("C-," . beginning-of-buffer)
              ("M-n" . outline-next-heading)
              ("M-p" . outline-previous-heading)
              ("M-h" . (lambda () (interactive) (org-latex-preview '(16))))
              ("M-q" . nil)
              ("C-)" . nil)
              ("C-)" . nil)
              ("C-#" . nil)
              ("C-'" . nil)
              ("C-M-t" . nil))
  :config
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted) 

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
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
   org-hide-emphasis-markers t
   org-ellipsis "…"
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
     (dot . t)
     (emacs-lisp . t)
     (haskell . t)))
  ;; Horrfic hack to disable highlight-indent-mode in python snippets
  ;; which are exported to html using org export.
  ;; See the defintion of `org-html-fontify-code' for why this works
  (defun python-no-elpy-mode ()
    (interactive)
    (let (python-mode-hook)
      (python-mode)))
  (add-to-list 'org-src-lang-modes '("python" . python-no-elpy))
  (add-to-list 'org-src-lang-modes '("haskell" . haskell-tng)))

(use-package org-contrib
  :after org)

;; https://github.com/magit/transient
(use-package transient)

;; https://github.com/Alexander-Miller/pfuture
(use-package pfuture)

(use-package ace-window
  :bind (("M-o" . ace-window)
         :map html-mode-map
         ("M-o" . nil)))

;; https://github.com/abo-abo/avy
;; https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  ;; This does two things: first, it creates an autoload for the avy-goto-char commands and defers loading of avy until you actually use it. Second, it binds the key C-: to that command.
  :bind
  ("C-:" . avy-goto-char)
  ("C-;" . avy-goto-char-timer)
  ("C-#" . avy-copy-region)
  ("M-#" . avy-copy-line)
  ("C-M-#" . avy-move-region)
  :config
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l)
        avy-keys-alist
        `((avy-goto-char-2 . (?a ?s ?d ?f ?j ?k ?l))
          (avy-goto-char-timer . (?a ?s ?d ?f ?j ?k ?l)))))

(use-package yasnippet
  :defer t
  ;; Allows for nested expansion
  :bind ("C-<tab>" . yas-expand)
  :init
  (autoload 'yasnippet "yasnippet" nil t)
  :config
  (yas-global-mode 1)
  (setq yas-indent-line 'fixed))

(use-package smartparens
  :demand t
  :init
  (defun lw-clone-line-lisp ()
    "Copy the current line to the next."
    (interactive)
    (let* ((p1 (progn (back-to-indentation) (point)))
           (p2 (progn (sp-forward-sexp) (point)))
           (contents (buffer-substring-no-properties p1 p2)))
      (forward-line -1)
      (end-of-line)
      (open-line 1)
      (forward-line)
      (insert contents)
      (indent-according-to-mode)))
  :hook ((helpful-mode . smartparens-mode)
         (messages-buffer-mode . smartparens-mode))
  :bind (:map smartparens-mode-map
              ("C-0" . sp-forward-slurp-sexp)
              ("C-9" . sp-forward-barf-sexp)
              ("C--" . sp-unwrap-sexp)
              ("C-r" . sp-up-sexp)
              ("M-r" . sp-backward-up-sexp)
              ("M-f" . sp-forward-sexp)
              ("M-b" . sp-backward-sexp)
              ("M-s" . sp-down-sexp)
              ("C-M-s" . (lambda () (interactive) (sp-down-sexp -1)))
              ("M-t" . sp-transpose-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-a" . sp-beginning-of-sexp)
              ("<C-backspace>" . lw-backword-kill-word-dwim)
              :map smartparens-strict-mode-map
              ("M-l" . lw-clone-line-lisp)
              :map emacs-lisp-mode-map
              (";" . sp-comment))
  :config
  (require 'smartparens-config)
  (setq sp-escape-quotes-after-insert t)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'slime-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'ielm-mode-hook #'smartparens-strict-mode)
  (add-hook 'minibuffer-inactive-mode-hook #'smartparens-mode)
  (add-hook 'minibuffer-mode-hook #'smartparens-mode)
  (smartparens-global-mode 1)

  (defun lw-backword-kill-word-dwim (arg)
    "Just to what I mean `backword-kill-word'!"
    (interactive "p")
    (let* ((start-of-line-point (save-mark-and-excursion
                                  (move-beginning-of-line nil)
                                  (point)))
           (string-before-point (buffer-substring-no-properties
                                 start-of-line-point
                                 (point))))
      (if (string-match-p "^\s+$" string-before-point)
          (kill-backward-chars (1+ (length string-before-point)))
        (sp-backward-kill-word arg))))

  ;; electric-pair-mode would do this for us but it doesn't play nicely with
  ;; smartparens
  ;; https://www.reddit.com/r/emacs/comments/f6vnya/how_to_add_a_newline_between_parentheses_brackets/

  (sp-with-modes
      '(python-mode scala-mode)
    (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "[" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :demand t
  ;;:load-path "~/projects/projectile"
  :init (require 'conf-mode)
  :bind (("C-c C-c" . projectile-test-project)
         ("C-c C-r" . projectile-run-project)
         :map conf-mode-map
         ("C-c C-c"))
  :config
  (projectile-mode 1)
  (setq projectile-create-missing-test-files t
        projectile-project-search-path '("~/projects")
        ;; https://github.com/bbatsov/projectile/issues/1517
        projectile-per-project-compilation-buffer t
        lw-all-ext
        '("yml" "yaml" "ini" "md" "xml" "jenkinsfile" "gql" "tf" "org" "conf" "gradle" "toml")
        lw-code-ext
        '("el" "py" "java" "hs" "scala" "sc" "sbt" "sh")
        projectile-other-file-alist
        (append projectile-other-file-alist
                `(("md"   . ,(append lw-all-ext lw-code-ext))
                  ("ini"   . ,(append lw-all-ext lw-code-ext))
                  ("yml"   . ,(append lw-all-ext lw-code-ext))
                  ("yaml"  . ,(append lw-all-ext lw-code-ext))
                  ("conf"  . ,(append lw-all-ext lw-code-ext))
                  ("el"    . ,(append lw-all-ext '("el" "md" "org")))
                  ("py"    . ,(append lw-all-ext '("py" "toml")))
                  ("toml"  . ,(append lw-all-ext '("py" "toml")))
                  ("java"  . ,(append lw-all-ext '("java" "gradle")))
                  ("scala" . ,(append lw-all-ext '("scala" "sc" "sbt")))
                  ("sc"    . ,(append lw-all-ext '("scala" "sc" "sbt")))
                  ("sbt"   . ,(append lw-all-ext '("scala" "sc" "sbt")))
                  ("hs"    . ,(append lw-all-ext '("hs" "cabal")))
                  ("cabal" . ,(append lw-all-ext '("hs" "cabal")))
                  ("org"   . ,(append lw-all-ext lw-code-ext))
                  ("gql"   . ,(append lw-all-ext lw-code-ext))))
        lw-eldev-related-files
        (list
         (projectile-related-files-fn-test-with-suffix "el" "-test")
         (projectile-related-files-fn-test-with-prefix "el" "test-")))
  
  (cl-defun lw-projectile-update-project-type-override
      (old-fn
       project-type
       &key precedence
       (marker-files nil marker-files-specified)
       (project-file nil project-file-specified)
       (compilation-dir nil compilation-dir-specified)
       (configure nil configure-specified)
       (compile nil compile-specified)
       (install nil install-specified)
       (package nil package-specified)
       (test nil test-specified)
       (run nil run-specified)
       (test-suffix nil test-suffix-specified)
       (test-prefix nil test-prefix-specified)
       (src-dir nil src-dir-specified)
       (test-dir nil test-dir-specified)
       (related-files-fn nil related-files-fn-specified)
       (test-file-fn nil test-file-fn-specified)
       (repl-fn nil repl-fn-specified))
    (apply old-fn
           (append (list project-type)
                   (when marker-files-specified `(:marker-files ,marker-files))
                   (when project-file-specified `(:project-file ,project-file))
                   (when compilation-dir-specified
                     `(:compilation-dir ,compilation-dir))
                   (when configure-specified `(:configure ,configure))
                   (when compile-specified `(:compile ,compile))
                   (when install-specified `(:install ,install))
                   (when package-specified `(:package ,package))
                   (when test-specified `(:test ,test))
                   (when run-specified `(:run ,run))
                   (when test-suffix-specified `(:test-suffix ,test-suffix))
                   (when test-prefix-specified `(:test-prefix ,test-prefix))
                   (when src-dir-specified `(:src-dir ,src-dir))
                   (when test-dir-specified `(:test-dir ,test-dir))
                   (when related-files-fn-specified
                     `(:related-files-fn ,related-files-fn))
                   `(:precedence ,precedence)))
    (setq projectile-project-types
          (--map-when (eq (car it) project-type)
                      (cons project-type
                            (projectile--combine-plists
                             (cdr it)
                             (append (when test-file-fn
                                       (list 'test-file-fn test-file-fn))
                                     (when repl-fn (list 'repl-fn repl-fn)))))
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
  (defun lw-projectile-repl ()
    (interactive)
    (if-let* ((p-type (projectile-project-type))
              (repl-fn (projectile-project-type-attribute p-type 'repl-fn))
              (current-file (buffer-file-name)))
        (funcall repl-fn current-file)
      (message "No repl strategy found for project type '%s'" p-type)))
  
  (define-key projectile-mode-map (kbd "C-c C-f") #'lw-projectile-test-file)
  (define-key projectile-mode-map (kbd "C-c C-i") #'lw-projectile-repl)
  (defun lw-sbt-test-file-fn (file-name)
    (interactive)
    (concat (lw-sbt-command) " 'testOnly "
            (lw-jvm-get-file-package (f-dirname file-name))
            "." (f-no-ext (f-filename file-name)) "'"))
  (defun lw-mill-test-file-fn (file-name)
    (interactive)
    (let* ((rel (f-relative file-name (projectile-project-root)))
           (mill-module
            (s-join "." (--take-while (not (string= it "src"))
                                      (f-split (f-no-ext rel))))))
      (format "mill %s.testOnly '%s.%s'"
              mill-module
              (lw-jvm-get-file-package (f-dirname file-name))
              (f-no-ext (f-filename file-name)))))

  (defun lw-send-to-current-eshell (command)
    ;; There's got to be a better way...
    (lw-maybe-projectile-eshell)
    (insert command)
    (eshell-send-input)
    (lw-eshell-clear-buffer))

  (defun lw-mill-repl (file-name)
    (interactive)
    (let* ((rel (f-relative file-name (projectile-project-root)))
           (mill-module
            (s-join "." (--take-while (not (string= it "src"))
                                      (f-split (f-no-ext rel))))))
      (lw-send-to-current-eshell (format "mill -i %s.repl" mill-module))))

  (defun lw-haskell-repl (file-name)
    (interactive)
    (let* ((rel (f-relative file-name (projectile-project-root)))
           (mill-module
            (s-join "." (--take-while (not (string= it "src"))
                                      (f-split (f-no-ext rel))))))
      (lw-send-to-current-eshell "stack ghci")))
  
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
   :run #'lw-sbt-run-cmd
   :test-file-fn #'lw-sbt-test-file-fn
   :precedence 'high)
  (projectile-update-project-type
   'mill
   :src-dir "src/"
   :test-dir "test/src/"
   :test-file-fn #'lw-mill-test-file-fn
   :repl-fn #'lw-mill-repl
   :precedence 'high)
  (projectile-update-project-type
   'maven
   :src-dir "main"
   :test-dir "test")
  (projectile-update-project-type
   'gradlew
   :test-suffix "Test"
   :src-dir "main"
   :test-dir "test")
  (projectile-update-project-type
   'haskell-stack
   :repl-fn #'lw-haskell-repl)
  (projectile-update-project-type
   'emacs-eldev
   :related-files-fn lw-eldev-related-files)
  (defun my-get-python-test-dir (impl-file-path)
    "Return the corresponding test file directory for IMPL-FILE-PATH"
    (let* ((rel-path (f-relative impl-file-path (projectile-project-root)))
           (src-dir (car (f-split rel-path))))
      (cond ((f-exists-p (f-join (projectile-project-root) "test"))
             (projectile-complementary-dir impl-file-path src-dir "test"))
            ((f-exists-p (f-join (projectile-project-root) "tests"))
             (projectile-complementary-dir impl-file-path src-dir "tests"))
            (t (error "Could not locate a test file for %s!" impl-file-path)))))

  (defun my-get-python-impl-dir (test-file-path)
    "Return the corresponding impl file directory for TEST-FILE-PATH"
    (if-let* ((root (projectile-project-root))
              (rel-path (f-relative test-file-path root))
              (src-dir-guesses
               `(,(f-base root) ,(s-replace "-" "_" (f-base root))
                 ,(downcase (f-base root)) "src"))
              (src-dir (cl-find-if (lambda (d) (f-exists-p (f-join root d)))
                                   src-dir-guesses)))
        (projectile-complementary-dir test-file-path "tests?" src-dir)
      (error "Could not locate a impl file for %s!" test-file-path)))
  
  (projectile-update-project-type
   'python-pkg
   :src-dir #'my-get-python-impl-dir
   :test-dir #'my-get-python-test-dir)
  (projectile-update-project-type
   'python-tox
   :src-dir #'my-get-python-impl-dir
   :test-dir #'my-get-python-test-dir))

;; http://tuhdo.github.io/helm-intro.html
(use-package helm
  :demand t
  :init
  (require 'org)
  :bind (("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-j" . helm-mini)
         ("C-x b" . helm-buffers-list)
         ("M-x" . helm-M-x)
         ("C-s" . helm-occur)
         :map helm-map
         ("C-," . helm-beginning-of-buffer)
         ("C-." . helm-end-of-buffer)
         ("C-k" . helm-buffer-run-kill-buffers)
         ("M-D" . helm-delete-minibuffer-contents)
         ("M-e" . helm-select-action)
         :map helm-occur-map
         ("C-s" . helm-next-line)
         ("C-r" . helm-previous-line)
         :map org-mode-map
         ("C-j" . helm-mini)
         :map lisp-interaction-mode-map
         ("C-j" . helm-mini)
         :map comint-mode-map
         ("C-M-r" . helm-comint-input-ring)
         ("M-e" . helm-select-action))
  :config
  (helm-mode 1)
  ;; Makes helm-boring-file-regexp-list act as a .gitignore
  (setq helm-ff-skip-boring-files t
        helm-M-x-fuzzy-match t
        helm-split-window-in-side-p t)
  (add-to-list 'savehist-additional-variables 'helm-M-x-input-history))

(use-package helm-flx
  :after helm
  :config
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t ; t by default
        helm-flx-for-helm-locate t))   ; nil by default

(use-package helm-ag
  :after helm
  :quelpa (helm-ag
           :fetcher github
           :repo "laurencewarne/helm-ag"
           :branch "skip-read-only-in-helm-ag-edit")
  :config
  (defun lw-helm-do-ag-current-directory ()
    (interactive)
    (let* ((ignored
            (mapconcat
             (lambda (i)
               (concat "--ignore " i))
             (append grep-find-ignored-files
                     grep-find-ignored-directories
                     (cadr (projectile-parse-dirconfig-file))) " "))
           (helm-ag-base-command (concat helm-ag-base-command " " ignored)))
      (helm-do-ag default-directory)))
  (define-key dired-mode-map (kbd "M-q") #'lw-helm-do-ag-current-directory))

(use-package helm-projectile
  ;; Don't add helm-ag to after because its loading is deferred
  :after (helm projectile)
  :demand t
  :init
  (require 'markdown-mode)
  :bind (:map python-mode-map
              ("M-k" . projectile-toggle-between-implementation-and-test)
              :map java-mode-map
              ("M-k" . projectile-toggle-between-implementation-and-test)
              :map emacs-lisp-mode-map
              ("M-k" . projectile-toggle-between-implementation-and-test))
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  ;; Here because :config in projectile is kinda... full
  (defun lw-switch-to-last-buffer ()
    "Switch to buffer an appropriate 'other buffer'."
    (interactive)
    (if (projectile-project-type)
        (let ((buf (cl-find-if
                    (lambda (p) (and (not (compilation-buffer-p p))
                                     (not (member p (mapcar #'window-buffer
                                                            (window-list))))))
                    (projectile-project-buffers))))
          (switch-to-buffer buf))
      (switch-to-buffer nil)))
  (defun lw-projectile-if-in-project (f1 f2)
    (lambda (&rest args)
      (interactive)
      (if (projectile-project-p)
          (apply f1 args)
        (apply f2 args))))

  (defun lw-switch-project ()
    (interactive)
    (let* ((projects (projectile-relevant-known-projects))
           (project (f-expand (completing-read "Switch to project: " projects)))
           (windows (window-list))
           (opened-files (projectile-project-buffers project))
           (all-files (if (> (length windows) (length opened-files))
                          (append
                           opened-files
                           (--> (projectile-project-files project)
                                (-take (- (length windows)(length opened-files)) it)
                                (-map (-partial #'f-join project) it)
                                (-map #'find-file-noselect it)))
                        opened-files)))
      (--each (-zip windows all-files)
        (set-window-buffer (car it) (cdr it)))))

  (define-key projectile-mode-map (kbd "C-j")
    (lw-projectile-if-in-project #'helm-projectile #'helm-mini))
  (define-key projectile-mode-map (kbd "M-q")
    (lw-projectile-if-in-project #'helm-projectile-ag #'helm-ag))
  (global-set-key (kbd "M-p") #'lw-switch-project)
  (define-key markdown-mode-map (kbd "M-p") #'lw-switch-project)
  (global-set-key (kbd "M-j") #'lw-switch-to-last-buffer))

(use-package helm-descbinds
  :after helm
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds))
  :config
  (setq helm-descbinds-window-style 'split-window))

(use-package company
  :demand t
  :bind ("M-RET" . company-complete)
  :config
  ;; We usually want make sure we have appropriate backends before enabling
  ;; lsp also appears to handle enabling company for its enabled modes
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'ielm-mode-hook 'company-mode)
  ;; See https://github.com/company-mode/company-mode/blob/master/NEWS.md
  (dolist (map (list company-active-map company-search-map))
    (define-key map (kbd "C-n") nil)
    (define-key map (kbd "C-p") nil)
    (define-key map (kbd "M-n") #'company-select-next)
    (define-key map (kbd "M-p") #'company-select-previous)))

;; https://www.flycheck.org/en/latest/index.html
;; https://www.flycheck.org/en/latest/user/flycheck-versus-flymake.html
(use-package flycheck
  :demand t
  :bind (:map flycheck-mode-map
              ("C-c e" . flycheck-next-error)
              ("C-c l" . flycheck-list-errors))
  ;; Don't use :hook here as that defers loading until flycheck is called
  :config
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

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

(use-package goto-chg
  :bind (("C-'" . goto-last-change)
         ("C-M-'" . goto-last-change-reverse)))

(use-package java-snippets
  :after yasnippet)

(use-package lsp-mode
  :hook
  (java-mode . lsp-deferred)
  (scala-mode . lsp-deferred)
  (lsp-mode . lsp-lens-mode)
  (scala-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t)))
  :bind ("C-M-<return>" . lsp-execute-code-action)
  :config
  (setq lsp-keep-workspace-alive nil
        lsp-enable-file-watchers nil
        lsp-enable-links nil
        lsp-headerline-breadcrumb-enable nil)
  (when-let* ((go-dir (concat (getenv "HOME") "/go/bin/sqls"))
              ((f-exists? go-dir)))
    (setq lsp-sqls-server go-dir)))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-show-with-cursor t))

(use-package lsp-java
  :after lsp-mode
  :config
  (setq lsp-java-format-comments-enabled nil
        lsp-java-format-on-type-enabled nil
        lsp-java-save-actions-organize-imports t)
  (setq tab-width 4))

(use-package helm-lsp
  :after lsp-mode helm
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;; https://www.mattduck.com/lsp-python-getting-started.html
(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t)
  (setq lsp-python-ms-python-executable (executable-find "python3"))
  :bind (:map lsp-signature-mode-map
              ("M-n" . nil))
  :hook (hack-local-variables . (lambda ()
		                  (when (derived-mode-p 'python-mode)
		                    (require 'lsp-python-ms)
		                    (lsp-deferred))))
  :config
  (defvar-local lw-python-no-shell nil)
  (defun lw-python-send-or-projectile (&optional send-main msg)
    "Send/create shell or run tests for project."
    (interactive)
    (let ((type (projectile-project-type)))
      (if (or (eq type 'python-tox) (eq type 'python-poetry) lw-python-no-shell)
          (call-interactively #'projectile-test-project)
        (lw-python-shell-send-buffer send-main msg))))
  (define-key python-mode-map (kbd "C-c C-c") 'lw-python-send-or-projectile))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

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
  :commands camcorder-record
  :bind (:map camcorder-mode-map
              ("C-<f12>" . camcorder-stop)
              ("C-M-k"   . camcorder-stop)))

;; https://github.com/purcell/package-lint
(use-package package-lint)

;; https://github.com/LaurenceWarne/jdoc-jumper
(use-package jdoc-jumper
  :ensure nil
  :quelpa (jdoc-jumper :fetcher github :repo "laurencewarne/jdoc-jumper" :upgrade t)
  ;; :load-path "~/projects/jdoc-jumper"
  :commands jdoc-jumper-jump-from-point)

;; https://github.com/anwyn/slime-company
(use-package slime-company
  :after company
  ;; We have to call this before slime is loaded:
  ;; https://github.com/anwyn/slime-company/issues/11
  :init
  (slime-setup '(slime-fancy slime-company)))

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
        slime-contribs '(slime-fancy)))

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

;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;; https://magit.vc/
(use-package magit
  :bind(("C-x g" . magit)
        ("C-c g" . magit-file-dispatch))
  :config
  (setq magit-clone-default-directory "~/projects")

  (defun lw-magit-checkout-last (&optional start-point)
    (interactive)
    (magit-branch-checkout "-" start-point))

  (transient-append-suffix 'magit-branch "w"
    '("-" "last branch" lw-magit-checkout-last))

  (define-advice magit-push-current-to-upstream (:before (args) query-yes-or-no)
    "Prompt for confirmation before permitting a push to upstream/master."
    (when-let ((branch (magit-get-current-branch))
               ((or (string= branch "master") (string= branch "main")))
               (remote (magit-get "branch" branch "remote"))
               ((string= remote "upstream")))
      (unless (yes-or-no-p (format "Push %s branch upstream to %s? "
                                   branch
                                   (or (magit-get-upstream-branch branch)
                                       (magit-get "branch" branch "remote"))))
        (user-error "Push to upstream aborted by user")))))

;; https://github.com/syohex/emacs-zoom-window
(use-package zoom-window
  :bind ("M-i" . zoom-window-zoom))

;; https://github.com/domtronn/all-the-icons.el
;; Note after installing this you need to run M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; https://github.com/sebastiencs/company-box
;; Need to M-x install-all-the-icons
(use-package company-box
  :after (company all-the-icons)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :config
  (global-set-key (kbd "M-'") 'er/expand-region))

(use-package magit-todos
  :after magit
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
  :config
  ;; Stolen from https://github.com/fniessen/org-html-themes
  (let* ((rto-css '("https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/htmlize.css"
                    "https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/readtheorg.css"))
         (rto-js '("https://fniessen.github.io/org-html-themes/src/lib/js/jquery.stickytableheaders.min.js"
                   "https://fniessen.github.io/org-html-themes/src/readtheorg_theme/js/readtheorg.js"))
         (extra-js '("https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"
                     "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js" ))
         (ox-yaow-html-head (concat (mapconcat (lambda (url) (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" url "\"/>\n")) rto-css "")
                                    (mapconcat (lambda (url) (concat "<script src=\"" url "\"></script>\n")) (append rto-js extra-js) ""))))
    (setq org-publish-project-alist
          (append
           `(("wiki-pages"
              :base-directory "~/org/"
              :base-extension "org"
              :publishing-directory "~/wiki/"
              :html-head ,ox-yaow-html-head
              :html-preamble t
              :recursive t
              :exclude ".*steam.*"
              :publishing-function ox-yaow-publish-to-html
              :preparation-function ox-yaow-preparation-fn
              :completion-function ox-yaow-completion-fn
              :ox-yaow-wiki-home-file "~/org/wiki.org"
              :ox-yaow-file-blacklist ("~/org/maths/answers.org")
              :ox-yaow-depth 2)
             ("wiki-static"
              :base-directory "~/org/"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
              :publishing-directory "~/wiki/"
              :recursive t
              :exclude ,(rx (0+ ".") (or "steam" "ltximg") (0+ "."))
              :publishing-function org-publish-attachment)
             ("wiki"
              :components ("wiki-pages" "wiki-static")))
           org-publish-project-alist))))

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
  :mode "\\.plantuml\\'"
  :config
  (require 'org)
  (require 'ob-plantuml)
  (setq plantuml-jar-path (concat user-emacs-directory "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)
  (unless (file-exists-p plantuml-jar-path)
    (plantuml-download-jar))
  (plantuml-set-exec-mode "jar")
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :config
  (which-key-mode))

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
  :after lsp lsp-ui scala-mode
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off")))

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
          ;; won't work in interactive buffers
          (local-set-key (kbd "q") 'kill-buffer-and-window)
          win))))
  (defalias 'lw-shackle-get-window-cur
    (lambda (buffer alist plist)
      (lw-shackle-get-window buffer alist plist nil)))

  (setq shackle-rules
        '((compilation-mode :select nil :custom lw-shackle-get-window)
          ("magit: .*" :regexp t :select t :custom lw-shackle-get-window-cur)
          (".*Org-Babel.*" :regexp t :select t :custom lw-shackle-get-window-cur)
          ("^\*eshell.*" :regexp t :select t :custom lw-shackle-get-window-cur)
          ("*cfw:details*" :select t :custom lw-shackle-get-window-cur)
          ("*HS-Error*" :select t :custom lw-shackle-get-window-cur)
          ("*Org Select*" :select t :custom lw-shackle-get-window-cur)
          ("*ASCII*" :select t :custom lw-shackle-get-window-cur)
          ("*Org Links*" :select t :custom lw-shackle-get-window-cur)
          ("*pytest*.*" :regexp t :custom lw-shackle-get-window-cur)
          ;;("\*docker.*" :regexp t :select t :custom lw-shackle-get-window-cur)
          ))
  (shackle-mode 1))

(use-package company-graphviz-dot
  :disabled
  :after company graphviz-dot-mode)

(defvar lw-eaf-location "~/.emacs.d/site-lisp/emacs-application-framework")

;; https://github.com/manateelazycat/emacs-application-framework#dependency-list
;; Note extra installation steps are required, see above link
(use-package eaf
  :demand t
  :if (and (eq system-type 'gnu/linux)
           (getenv "DBUS_SESSION_BUS_ADDRESS")
           (f-exists-p lw-eaf-location))
  :load-path lw-eaf-location
  :custom
  ;;See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t))

;; https://github.com/emacs-eaf/eaf-pdf-viewer
;; "pip3 install PyMuPDF --user" appears to make this work
(use-package eaf-pdf-viewer
  :demand t
  :if (and (eq system-type 'gnu/linux)
           (getenv "DBUS_SESSION_BUS_ADDRESS")
           (f-exists-p lw-eaf-location))
  :load-path "~/.emacs.d/site-lisp/eaf-pdf-viewer"
  :config
  (setq eaf-pdf-dark-mode nil)
  (setq eaf-pdf-show-progress-on-page nil)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up_page "n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_begin "C-," eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_end "C-." eaf-pdf-viewer-keybinding)
  (eaf-bind-key jump_to_page "M-g M-g" eaf-pdf-viewer-keybinding)
  (eaf-bind-key kill-this-buffer "k" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-u" eaf-pdf-viewer-keybinding)
  (add-to-list 'org-file-apps '("pdf" . (lambda (f _) (eaf-open f "pdf-viewer")))))

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
  :bind (("C-c d" . docker)
         :map docker-container-mode-map
         ("q" . kill-current-buffer)
         ("k" . kill-current-buffer)
         :map docker-image-mode-map
         ("q" . kill-current-buffer)
         ("k" . kill-current-buffer)
         :map docker-network-mode-map
         ("q" . kill-current-buffer)
         ("k" . kill-current-buffer)
         :map docker-volume-mode-map
         ("q" . kill-current-buffer)
         ("k" . kill-current-buffer)
         :map tablist-mode-map
         ("k" . nil)
         :map tablist-minor-mode-map
         ("k" . nil))
  :config
  (setq docker-show-messages nil)
  (add-to-list
   'docker-image-run-custom-args
   `("^postgres" ("-e POSTGRES_PASSWORD=postgres" . ,docker-image-run-default-args)))
  (add-to-list
   'docker-image-run-custom-args
   `(".*url-to-pdf.*"
     ("-d" "--name url2pdf" "-p 80:80" . ,docker-image-run-default-args)))
  (add-to-list
   'docker-image-run-custom-args
   `(".*jaegertracing.*"
     ;; For the Jaeger UI open:
     ;; http://localhost:16686/
     ("-d" "--name jaeger" "-e COLLECTOR_ZIPKIN_HOST_PORT=:9411" "-p 80:80 -p 5775:5775/udp -p 6831:6831/udp -p 6832:6832/udp -p 5778:5778 -p 16686:16686 -p 14268:14268 -p 14250:14250 -p 9411:9411" . ,docker-image-run-default-args))))

;; https://github.com/jcs-elpa/goto-line-preview
(use-package goto-line-preview
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

;; https://github.com/jorgenschaefer/emacs-buttercup
;; We install this package to get the correct indentation for `describe' and
;; `it' blocks when writing tests.
(use-package buttercup)

;; https://github.com/LaurenceWarne/finito.el
(use-package finito
  :demand t
  ;;:ensure nil
  ;;:quelpa (finito :fetcher github :repo "laurencewarne/finito.el" :upgrade t)
  ;;:load-path "~/projects/finito.el"
  :bind (("C-c b" . finito)
         :map finito-collection-view-mode-map
         ("x" . finito-delete-data-for-book-at-point))
  :config
  (finito-download-server-if-not-exists
   (lambda () (finito-start-server-if-not-already)))
  ;; Gives coloured output to finito process buffer
  ;; See: https://stackoverflow.com/questions/44348443/ansi-coloring-in-emacs-start-process-output-buffer
  (add-hook
   'buffer-list-update-hook
   (lambda ()
     (when-let ((buf (get-buffer "finito"))
                (proc (get-process "finito")))
       (with-current-buffer buf
         (unless (bound-and-true-p finito-coloured)
           (ansi-color-for-comint-mode-on)
           (comint-mode)
           (set-process-filter proc 'comint-output-filter)
           (setq-local finito-coloured t))))))
  (setq transient-display-buffer-action '(display-buffer-below-selected)))

;; https://github.com/davazp/graphql-mode
(use-package graphql-mode
  :after projectile
  :bind (:map graphql-mode-map
              ("C-c C-c" . nil)
              ("C-j" . helm-projectile)
              ("M-q" . helm-projectile-ag)))

;; https://github.com/vermiculus/graphql.el
(use-package graphql)

;; https://github.com/akicho8/string-inflection
(use-package string-inflection
  :bind ("C-=" . string-inflection-all-cycle))

;; https://polymode.github.io/
(use-package polymode
  :after org
  :config
  ;; https://emacs.stackexchange.com/questions/33684/proper-way-to-change-prefix-key-for-minor-mode-map?rq=1
  (define-key polymode-mode-map (kbd "C-c n")
    (lookup-key polymode-mode-map (kbd "M-n")))
  (define-key polymode-mode-map (kbd "M-n") nil)
  
  ;; ;; See https://polymode.github.io/defining-polymodes/
  ;; (define-hostmode poly-org-hostmode :mode 'org-mode)
  ;; ;; Note we don't use the :head-mode and :tail-mode options since it messes
  ;; ;; up rainbow delims and the default poly-head-tail-mode used to fontify
  ;; ;; is fine
  ;; (define-innermode poly-latex-multiline-expr-org-innermode
  ;;   :mode 'LaTeX-mode
  ;;   :head-matcher (rx "\\\[")
  ;;   :tail-matcher (rx "\\\]"))
  ;; (define-innermode poly-latex-expr-org-innermode
  ;;   :mode 'LaTeX-mode
  ;;   :head-matcher (rx "\\\(")
  ;;   :tail-matcher (rx "\\\)"))
  ;; (define-polymode poly-org-latex-mode
  ;;   :hostmode 'poly-org-hostmode
  ;;   :innermodes '(poly-latex-multiline-expr-org-innermode
  ;;                 poly-latex-expr-org-innermode))
  )

;; https://polymode.github.io/
(use-package poly-markdown
  :hook (markdown-mode . poly-markdown-mode))

;; https://github.com/mattiase/xr
(use-package xr)

;; https://www.gnu.org/software/auctex/manual/auctex/index.html
(use-package tex
  :ensure auctex)

(use-package company-auctex
  :after tex
  :hook (TeX-mode . company-mode)
  :config
  (company-auctex-init))

;; https://github.com/magit/git-modes
(use-package git-modes
  :mode ("/.dockerignore\\'" . gitignore-mode))

;; https://github.com/abo-abo/hydra
(use-package hydra
  :bind (("C-c f" . hydra-flycheck/body)
         ("C-c p" . hydra-profiler/body)
         ("C-c a" . hydra-macro/body))
  :config
  (defhydra hydra-macro (:hint nil :color pink :pre 
                               (when defining-kbd-macro
                                 (kmacro-end-macro 1)))
    "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register     
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_k_^           [_m_] step
    ^^   ^^          [_s_] swap"
    ("j" kmacro-start-macro :color blue)
    ("l" kmacro-end-or-call-macro-repeat)
    ("i" kmacro-cycle-ring-previous)
    ("k" kmacro-cycle-ring-next)
    ("r" apply-macro-to-region-lines)
    ("d" kmacro-delete-ring-head)
    ("e" kmacro-end-or-call-macro-repeat)
    ("o" kmacro-edit-macro-repeat)
    ("m" kmacro-step-edit-macro)
    ("s" kmacro-swap-ring)
    ("n" kmacro-insert-counter)
    ("t" kmacro-set-counter)
    ("a" kmacro-add-counter)
    ("f" kmacro-set-format)
    ("b" kmacro-name-last-macro)
    ("K" kmacro-bind-to-key)
    ("B" insert-kbd-macro)
    ("x" kmacro-to-register)
    ("'" kmacro-edit-macro)
    ("," edit-kbd-macro)
    ("q" nil :color blue))

  (defhydra hydra-flycheck
    (:pre (flycheck-list-errors)
          :post (quit-windows-on "*Flycheck errors*")
          :hint nil)
    "Errors"
    ("f" flycheck-error-list-set-filter "Filter")
    ("n" flycheck-next-error "Next")
    ("p" flycheck-previous-error "Previous")
    ("gg" flycheck-first-error "First")
    ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q" nil))

  (defun profiler-running-modes ()
    (let ((running-modes
           (-non-nil (list (if (profiler-cpu-running-p) "cpu")
                           (if (profiler-memory-running-p) "mem")))))
      (if running-modes
          (s-join "+" running-modes)
        "stopped")))

  (defhydra hydra-profiler
    (:color red :hint nil)
    "
elisp profiling (currently %s(profiler-running-modes))

^^Start / stop                          Reporting
^-^----------------------------------   ^-^----------------------------
_s_: start (prompt for mode)            _r_: show report
_c_: start CPU profiling
_m_: start memory profiling             _f_: find profile
_b_: start both CPU+memory profiling    _4_: find profile other window
_._: stop profiling                     _5_: find profile other frame
_R_: reset profiler logs

_q_: quit
_C_: customize profiler options
"
    ("s" hydra-profiler/profiler-start)
    ("c" (hydra-profiler/profiler-start 'cpu))
    ("m" (hydra-profiler/profiler-start 'mem))
    ("b" (hydra-profiler/profiler-start 'cpu+mem))
    ("." hydra-profiler/profiler-stop)
    ("R" profiler-reset)
    ("q" nil)
    ("C" (customize-group "profiler"))
    ("r" profiler-report :color blue)
    ("f" profiler-find-profile)
    ("4" profiler-find-profile-other-window)
    ("5" profiler-find-profile-other-frame)))

(use-package eshell-prompt-extras
  :demand t
  :bind (:map eshell-mode-map
              ("C-M-r" . helm-eshell-history))
  :custom-face
  (epe-pipeline-delimiter-face ((t :foreground "light green")))
  (epe-pipeline-user-face ((t :foreground "aquamarine"
                              :weight bold)))
  (epe-pipeline-host-face ((t :foreground "lawn green")))
  :hook ((eshell-mode . (lambda () (setq lw-unix-line-discard-bol-fn
                                         #'eshell-bol)))
         (eshell-mode . smartparens-mode))
  :config
  (setq eshell-prompt-function #'epe-theme-pipeline
        eshell-hist-ignoredups 'erase
        epe-pipeline-show-time nil)
  (defun lw-eshell-clear-buffer ()
    "Clear eshell buffer."
    (interactive)
    (let ((inhibit-read-only t)
          (line (buffer-substring-no-properties
                 (progn (eshell-bol) (point))
	         (progn (end-of-line) (point)))))
      (erase-buffer)
      (eshell-send-input)
      (insert line)))
  (defun lw-eshell-delete-char-or-exit (&optional killflag)
    "Call `delete-char' or exit the buffer + window if there is no forward char."
    (interactive)
    (condition-case nil
        (delete-char 1 killflag)
      (end-of-buffer
       (kill-buffer-and-window))))
  (defun lw-maybe-projectile-eshell (arg)
    "Call `projectile-run-eshell' if within a project, else `eshell'.

If a prefix argument is present, run `eshell' without checking if the current
directory is part of a projectile project."
    (interactive "P")
    ;; Don't need :after projectile
    (if (and (fboundp #'projectile-project-root) (projectile-project-root)
             (or (not (numberp (car-safe arg))) (/= (car-safe arg) 4)))
        (projectile-run-eshell)
      (eshell)))
  (define-key global-map (kbd "C-M-t") #'lw-maybe-projectile-eshell)
  ;; https://lists.gnu.org/r/bug-gnu-emacs/2019-06/msg01616.html
  (add-hook
   'eshell-mode-hook
   (lambda ()
     (define-key eshell-mode-map (kbd "C-l") #'lw-eshell-clear-buffer)
     (define-key eshell-mode-map (kbd "C-d") #'lw-eshell-delete-char-or-exit))
   99))

;; https://gitlab.com/tseenshe/haskell-tng.el
(use-package haskell-tng-mode
  :ensure nil
  :quelpa (haskell-tng-mode :fetcher gitlab :repo "tseenshe/haskell-tng.el")
  :mode ((rx ".hs" eos) . haskell-tng-mode)
  :bind
  (:map
   haskell-tng-mode-map
   ("RET" . haskell-tng-newline)
   ("C-c c" . haskell-tng-compile)
   ("C-c e" . next-error))
  :config
  (require 'haskell-tng-hsinspect)
  (require 'haskell-tng-extra)
  (require 'haskell-tng-extra-hideshow)
  (require 'haskell-tng-extra-company)
  (require 'haskell-tng-extra-projectile)
  (require 'haskell-tng-extra-smartparens)
  ;;(require 'haskell-tng-extra-yasnippet)

  (defun lw-haskell-send-or-projectile (&optional send-main msg)
    "Send/create shell or run tests for project."
    (interactive)
    (let ((type (projectile-project-type)))
      (if (or (eq type 'haskell-stack) (eq type 'haskell-cabal))
          (call-interactively #'projectile-test-project)
        (let ((file-name (buffer-file-name)))
          (if-let ((buf (get-buffer "*ghci*")))
              (progn (comint-send-string (get-buffer-process buf)
                                         (format ":l \"%s\"" file-name))
                     (other-window 1)
                     (switch-to-buffer buf)
                     (comint-send-input))
            (other-window 1)
            (comint-run "ghci" (list file-name)))))))

  (defun lw-haskell-bol-from-prev ()
    (interactive)
    (when (save-excursion
            (forward-line -1)
            (beginning-of-line)
            (looking-at-p (rx eol)))
      (beginning-of-line)
      (kill-line)))

  ;; (advice-add 'haskell-tng-newline
  ;;             :after
  ;;             #'lw-haskell-bol-from-prev)
  (define-key haskell-tng-mode-map (kbd "C-c C-c") 'lw-haskell-send-or-projectile))

;; https://github.com/emacs-lsp/lsp-haskell
(use-package lsp-haskell
  :hook (haskell-tng-mode . lsp-deferred))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (nxml-mode . aggressive-indent-mode)))

(use-package ts)

(use-package yaml)

(use-package adoc-mode
  :mode "\\.adoc\\'")

;; https://github.com/xiongtx/eros
(use-package eros
  :config
  (eros-mode 1))

(use-package eldev
  :mode (("/Eldev\\'" . emacs-lisp-mode)
         ("/Eldev-local\\'" . emacs-lisp-mode)))

;; https://github.com/Lindydancer/font-lock-studio
(use-package font-lock-studio)

;; https://github.com/LaurenceWarne/prefab.el
(use-package prefab
  :bind ("C-c c" . prefab)
  :config
  (setq prefab-cookiecutter-python-executable
        "~/.local/pipx/venvs/cookiecutter/bin/python3"
        prefab-debug t))

;; https://melpa.org/#/ascii-table
;; Press 'b' for binary, 'o' for octal, 'd' for decimal and 'x' for hexadecimal.
(use-package ascii-table
  :commands ascii-table)

;; https://github.com/purcell/whole-line-or-region
(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-global-mode))

(use-package saws
  :if (f-exists-p "~/projects/saws.el")
  :load-path "~/projects/saws.el")

;; https://github.com/casouri/undo-hl
(use-package undo-hl
  :ensure nil
  :quelpa (undo-hl :fetcher github :repo "casouri/undo-hl")
  :hook ((text-mode . undo-hl-mode)
         (prog-mode . undo-hl-mode)))

;; https://github.com/pythonic-emacs/blacken/blob/master/blacken.el
(use-package blacken
  :after python
  :custom (blacken-only-if-project-is-blackened t)
  :hook (python-mode . blacken-mode))

;; https://github.com/wbolster/emacs-python-pytest
(use-package python-pytest
  :config
  (let ((python-version
         (string-trim-right
          (shell-command-to-string
           "python3 -c 'import sys;print(f\"{sys.version_info.major}.{sys.version_info.minor}\")'"))))

    (defun lw-pytest-test-file-fn (file-name)
      (interactive)
      (let* ((rel (f-relative file-name (projectile-project-root)))
             (test-str (projectile-project-type-attribute
                        (projectile-project-type)
                        'test-command)))
        (format "%s -- %s" test-str rel)))

    (projectile-update-project-type
     'python-poetry
     :src-dir #'my-get-python-impl-dir
     :test-dir #'my-get-python-test-dir
     :test (format "nox -R --session tests-%s" python-version)
     :test-file-fn #'lw-pytest-test-file-fn)))

;; https://github.com/wyuenho/emacs-python-isort
(use-package python-isort)

;; https://github.com/minad/org-modern
(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;; https://github.com/emacsmirror/undo-fu-session
(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

;; https://github.com/mkcms/interactive-align
(use-package ialign
  :commands ialign
  :bind (("C-x l" . ialign)
         :map ialign-minibuffer-keymap
         ("C--" . ialign-decrement-spacing)
         ("C-=" . ialign-increment-spacing)
         ("C-n" . ialign-increment-group)
         ("C-p" . ialign-decrement-group))
  :config
  (setq ialign-initial-repeat t)
  (defun lw-ialign (old-fn beg end &optional regexp group spacing repeat)
    (interactive "r")
    (let ((overriding-terminal-local-map ialign-minibuffer-keymap))
      (funcall old-fn beg end regexp group spacing repeat)))
  (advice-add 'ialign :around #'lw-ialign))

;; https://github.com/LaurenceWarne/lsp-cfn.el
(use-package lsp-cfn
  :magic (("\\({\n *\\)? *[\"']AWSTemplateFormatVersion" . lsp-cfn-json-mode)
          ("\\({\n *\\)? *[\"']Transform[\"']: [\"']AWS::Serverless-2016-10-31" . lsp-cfn-json-mode)
          ("\\(---\n\\)?AWSTemplateFormatVersion:" . lsp-cfn-yaml-mode)
          ("\\(---\n\\)?Transform: AWS::Serverless-2016-10-31" . lsp-cfn-yaml-mode))
  :hook ((lsp-cfn-yaml-mode . lsp-deferred)
         (lsp-cfn-json-mode . lsp-deferred))
  :config
  (setq completion-ignore-case t)
  (setq lsp-cfn-verbose t)
  (define-key lsp-cfn-yaml-mode-map (kbd "C-c C-c") #'saws-deploy))

;; https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode)

;; https://github.com/yjwen/org-reveal/
(use-package ox-reveal
  :config
  (setq org-reveal-root
        (or (-some--> (-first #'f-exists-p '("~/repos/reveal.js/"
                                             "~/projects/reveal.js/"))
              (concat "file://" (f-expand it)))
            org-reveal-root))
  (setq org-reveal-title-slide "<h1>%t</h1><h2>%a</h2><p>%d<p>"))

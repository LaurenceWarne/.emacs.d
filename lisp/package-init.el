;;;; package-init.el -- installs/initializes packages -*- lexical-binding: t -*-
;;; Commentary:

;; Use use-package to install and configure packages in a readable way.
;; See:
;; https://github.com/jwiegley/use-package
;; https://jwiegley.github.io/use-package/keywords/

;;;; Code:

;; https://elpa.gnu.org/packages/delight.html
(use-package delight)

;; https://github.com/quelpa/quelpa
(use-package quelpa
  :init
  (setq quelpa-checkout-melpa-p nil
        quelpa-update-melpa-p nil)  ;; What does quelpa use melpa for?
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

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(x mac ns))
    ;;(delete "-i" exec-path-from-shell-arguments)
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
              ("C-j" . nil)
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
      "Weekly Org Entries")
     ("SachaChua"
      "https://sachachua.com/blog/feed/"
      "~/org/feeds.org"
      "Emacs News")
     ("Ponder This"
      "https://research.ibm.com/haifa/ponderthis/rss/index.xml"
      "~/org/feeds.org"
      "Monthly Ponder This Puzzles")
     ("SomethingSomethingProgramming"
      "https://nickdrozd.github.io/feed.xml"
      "~/org/feeds.org"
      "Something Something Programming")
     ("BusyBeaver"
      "https://www.sligocki.com/feed.xml"
      "~/org/feeds.org"
      "Busy Beaver"))
   org-confirm-babel-evaluate nil
   org-babel-python-command (-first #'executable-find '("python3" "python")))
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
  :delight (yas-global-mode) (yas-minor-mode)
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
              ("C-d" . lw-sp-strict-delete-char-unless-string)
              ("<backspace>" . lw-sp-strict-backward-delete-char-unless-string)
              ("C-w" . lw-sp-strict-kill-region-unless-string)
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
  (add-hook 'emacs-startup-hook
            (lambda () (when-let ((buf (get-buffer "*scratch*")))
                         (with-current-buffer buf
                           (smartparens-strict-mode -1)))))
  (smartparens-global-mode 1)

  (defun lw-backword-kill-word-dwim (arg)
    "Just do what I mean `backword-kill-word'!"
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

  (defun lw-sp-strict-backward-delete-char-unless-string (&optional arg)
    (interactive "P")
    (cond (mark-active (sp-delete-region (region-beginning) (region-end)))
          ((/= (or (nth 8 (syntax-ppss)) (1- (point))) (1- (point)))
           (delete-backward-char 1))
          (t (sp-backward-delete-char arg))))

  (defun lw-sp-strict-delete-char-unless-string (&optional arg)
    (interactive)
    (if (/= (or (when-let ((opening-quote (nth 8 (syntax-ppss))))
                  (save-mark-and-excursion
                    (goto-char opening-quote)
                    (sp-forward-sexp)
                    (point))) (1+ (point))) (1+ (point)))
        (delete-char 1)
      (sp-delete-char arg)))

  (defun lw-sp-strict-kill-region-unless-string (beg end)
    (interactive "r")
    (if (nth 3 (syntax-ppss))
        (kill-region beg end)
      (sp-kill-region beg end))))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :demand t
  :delight '(:eval (format " P[%s]" (projectile-project-type)))
  ;;:load-path "~/projects/projectile"
  :init (require 'conf-mode)
  :bind (("C-c C-c" . projectile-test-project)
         ("C-c C-r" . projectile-run-project)
         ("M-k" . projectile-toggle-between-implementation-and-test)
         :map conf-mode-map
         ("C-c C-c" . nil))
  :config
  (projectile-mode 1)
  (setq projectile-create-missing-test-files t
        projectile-auto-discover nil
        projectile-project-search-path '("~/projects")
        ;; https://github.com/bbatsov/projectile/issues/1517
        projectile-per-project-compilation-buffer t
        lw-all-ext
        '("yml" "yaml" "ini" "md" "xml" "jenkinsfile" "gql" "tf" "org" "conf" "gradle" "toml" "css")
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
          (funcall-interactively f1 args)
        (funcall-interactively f2 args))))

  (defun lw-switch-project ()
    (interactive)
    (let* ((projects (projectile-relevant-known-projects))
           (project (f-expand (completing-read "Switch to project: " projects)))
           (windows (window-list))
           (prog-file-p (lambda (f) (or (f-ext-p f "py") (f-ext-p f "scala")
                                        (f-ext-p f "sc") (f-ext-p f "el"))))
           (opened-files (-filter #'buffer-file-name
                                  (projectile-project-buffers project)))
           (all-files (if (> (length windows) (length opened-files))
                          (append
                           opened-files
                           (--> (projectile-project-files project)
                                (-flatten
                                 (-separate
                                  (lambda (f)
                                    (or (f-ext-p f "py") (f-ext-p f "scala")
                                        (f-ext-p f "sc") (f-ext-p f "el"))) it))
                                (-take (- (length windows) (length opened-files)) it)
                                (-map (-partial #'f-join project) it)
                                (-map #'find-file-noselect it)))
                        opened-files)))
      (--each (-zip windows all-files)
        (set-window-buffer (car it) (cdr it)))))

  ;; (define-key projectile-mode-map (kbd "C-j")
  ;;   (lw-projectile-if-in-project #'helm-projectile #'helm-mini))
  ;; (define-key projectile-mode-map (kbd "M-q")
  ;;   (lw-projectile-if-in-project #'helm-projectile-ag #'helm-ag))
  (global-set-key (kbd "M-p") #'lw-switch-project)
  (require 'markdown-mode)
  (define-key markdown-mode-map (kbd "M-p") #'lw-switch-project)
  (global-set-key (kbd "M-j") #'lw-switch-to-last-buffer)
  
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
    (let* ((split (f-split (f-relative file-name (projectile-project-root))))
           (module-guess (unless (string= "src" (car split))
                           (car (last (car (-split-on "src" split))))))
           (module-str (if module-guess (concat module-guess "/") "")))
      (format "%s '%stestOnly %s.%s'"
              (lw-sbt-command)
              module-str
              (lw-jvm-get-file-package (f-dirname file-name))
              (f-no-ext (f-filename file-name)))))
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
   :test-dir #'my-get-python-test-dir)

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

(use-package company
  :demand t
  :delight company-mode
  :bind ("M-RET" . company-complete)
  ;; We usually want make sure we have appropriate backends before enabling
  ;; lsp also appears to handle enabling company for its enabled modes
  :hook ((eshell-mode . company-mode)  ; TODO https://www.emacswiki.org/emacs/EshellCompletion
         (emacs-lisp-mode . company-mode)
         (ielm-mode . company-mode)
         (LaTeX-mode . company-mode))
  :config
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

;; Note groovy mode automatically adds itself to auto-mode-alist
(use-package groovy-mode
  :mode "\\.groovy\\'"
  :bind (:map groovy-mode-map
              ("M-k" . projectile-toggle-between-implementation-and-test)))

;; https://github.com/dakra/speed-type
(use-package speed-type
  :commands (speed-type-text speed-type-region speed-type-buffer)
  :config
  (setq speed-type-default-lang 'English))

(use-package goto-chg
  :bind (("C-'" . goto-last-change)
         ("C-M-'" . goto-last-change-reverse)))

;; https://github.com/emacs-lsp/lsp-mode
;; The following may be useful:
;;   - `lsp-describe-session': Shows the server capabilities
;;   - `lsp-doctor': Show lsp performance configuration (https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   - (setq lsp-log-io t), `lsp-workspace-show-log': Shows all communication between
;;     the server and client
;;
;; See also
;; https://github.com/emacs-lsp/lsp-mode/blob/master/docs/tutorials/how-to-turn-off.md
(use-package lsp-mode
  ;; :load-path "~/projects/lsp-mode"
  :delight lsp-lens-mode
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-M-<return>" . lsp-execute-code-action)
              ("M-e" . lsp-avy-lens)
              :map lsp-browser-mode-map
              ("k" . kill-current-buffer))
  :config
  (setq lsp-keep-workspace-alive nil
        lsp-enable-file-watchers nil
        lsp-enable-links nil
        ;; lsp-lens-enable nil
        lsp-headerline-breadcrumb-enable nil)
  (when-let* ((go-dir (concat (getenv "HOME") "/go/bin/sqls"))
              ((f-exists? go-dir)))
    (setq lsp-sqls-server go-dir))

  (advice-add
   #'lsp--text-document-position-params
   :around
   (defun my-text-pos-ext (old-fn &optional identifier position)
     (append
      (funcall old-fn identifier position)
      (when (region-active-p)
        (list :range (lsp--region-to-range (region-beginning) (region-end))))))))

;; https://github.com/emacs-lsp/dap-mode
(use-package dap-mode
  :config
  (defun lw-dap-go-to-output-buffer (&optional no-select)
    "Go to output buffer."
    (interactive)
    (let* ((buf (dap--debug-session-output-buffer (dap--cur-session-or-die)))
           (win (display-buffer-below-selected
                 buf
                 `((side . bottom) (slot . 5) (window-height . 0.50)))))
      (with-current-buffer buf (compilation-mode 1))
      (set-window-dedicated-p win t)
      (unless no-select (select-window win))))
  (advice-add 'dap-go-to-output-buffer :override #'lw-dap-go-to-output-buffer))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-show-with-cursor t))

(use-package lsp-java
  :hook
  (java-mode . lsp-deferred)
  :config
  (setq lsp-java-format-comments-enabled nil
        lsp-java-format-on-type-enabled nil
        lsp-java-save-actions-organize-imports t)
  (setq tab-width 4))

;; https://www.mattduck.com/lsp-python-getting-started.html
(use-package lsp-python-ms
  :after python
  :init
  (setq lsp-python-ms-auto-install-server t)
  (setq lsp-python-ms-python-executable (executable-find "python3"))
  :bind (:map lsp-signature-mode-map
              ("M-n" . nil)
              ("M-p" . nil)
              :map python-mode-map
              ("C-c C-c" . lw-python-send-or-projectile))
  :hook (hack-local-variables . (lambda ()
		                  (when (and (not (eq major-mode 'sage-shell:sage-mode))
                                             (derived-mode-p 'python-mode))
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
        (lw-python-shell-send-buffer send-main msg)))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; https://depp.brause.cc/eyebrowse/
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

;; https://github.com/purcell/package-lint
(use-package package-lint
  :commands package-lint-current-buffer)

;; https://github.com/slime/slime
(use-package slime
  :commands slime
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
        slime-contribs '(slime-fancy)))

;; https://github.com/anwyn/slime-company
(use-package slime-company
  :after (slime company)
  ;; We have to call this before slime is loaded:
  ;; https://github.com/anwyn/slime-company/issues/11
  :init
  (slime-setup '(slime-fancy slime-company)))

(use-package steam
  ;; :load-path "~/projects/steam.el"
  :commands (steam-get-games steam-launch)
  :config
  (setq steam-username "39422361280608732623190235"))

(use-package beacon
  :delight beacon-mode
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
         ("C-h k" . helpful-key)
         :map helpful-mode-map
         ("n" . help-go-forward)
         ("p" . help-go-back)))

;; https://magit.vc/
(use-package magit
  :bind(("C-x g" . magit)
        ("C-c g" . magit-file-dispatch))
  :config
  (setq magit-clone-default-directory "~/projects"
        magit-no-confirm '(set-and-push stage-all-changes unstage-all-changes))

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
(use-package all-the-icons
  :if (display-graphic-p))

;; https://github.com/sebastiencs/company-box
;; Need to M-x install-all-the-icons
(use-package company-box
  :delight company-box-mode
  :after (company all-the-icons)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind ("M-'" . er/expand-region))

(use-package magit-todos
  :after magit
  :load-path "~/projects/magit-todos"
  :config
  (magit-todos-mode))

(use-package mc-biome-viewer
  :ensure nil
  ;; :load-path "~/projects/mc-biome-viewer"
  :quelpa (mc-biome-viewer :fetcher github :repo "LaurenceWarne/mc-biome-viewer" :upgrade t)
  :commands (mc-biome-viewer-view-save mc-biome-viewer-view-seed)
  ;; Example configuration
  :config
  (setq mc-biome-viewer-column-chunks-in-camera 48)  ; But fewer chunks will be faster
  (puthash "ice plains" '(:foreground "silver") mc-biome-viewer-biome-to-face-map))

;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :demand t
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; https://github.com/ardumont/org2jekyll
(use-package org2jekyll
  :config
  (setq org2jekyll-blog-author "Laurence Warne"
        org2jekyll-source-directory (expand-file-name "~/projects/posts/")
        org2jekyll-jekyll-directory (expand-file-name "~/projects/LaurenceWarne.github.io/")
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

;; https://github.com/wyuenho/all-the-icons-dired
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; https://github.com/Fuco1/dired-hacks
(use-package dired-filter
  :hook (dired-mode . dired-filter-group-mode)
  :config
  (setq dired-filter-group-saved-groups
        '(("default"
           ("Directories"
            (directory))
           ("Git"
            (regexp . "^\\.git"))
           ("Python"
            (extension . "py"))
           ("Java"
            (extension . "java"))
           ("Scala"
            (extension "scala" "sc"))
           ("Lisp"
            (extension "el" "cl" "elc"))
           ("Org"
            (extension . "org"))
           ("PDF"
            (extension . "pdf"))
           ("LaTeX"
            (extension "tex" "bib"))
           ("HTML"
            (extension . "html"))
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
  :after dired
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

;; https://github.com/purcell/diredfl
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

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

;; https://github.com/positron-solutions/command-log-mode
(use-package command-log-mode
  :quelpa (command-log-mode :fetcher github :repo "https://github.com/positron-solutions/command-log-mode")
  :commands clm/toggle
  :custom
  (clm-window-text-scale 2 "Command log two steps higher text scale")
  (clm-logging-shows-buffer t "Toggling will show the buffer.")
  (clm-hiding-disables-logging t "Toggling visible buffer turns off logging.")
  (clm-disabling-logging-kills-buffer t "The buffer will be new when displayed again.")
  (clm-log-globally t "Auto-enable with global minor mode (including minibuffer)")
  (clm-exceptions '(self-insert-command) "Be chatty.
   Show everything besides self-insert-command"))

;; https://github.com/sagemath/sage-shell-mode
(use-package sage-shell-mode
  :commands sage-shell:run-sage)

;; https://github.com/Fuco1/fontify-face
(use-package fontify-face
  :delight fontify-face-mode
  :hook (emacs-lisp-mode . fontify-face-mode))

;; https://github.com/cireu/elispfl
(use-package elispfl
  :ensure nil
  :quelpa (elispfl :fetcher github :repo "cireu/elispfl")
  :hook ((emacs-lisp-mode . elispfl-mode)
         (ielm-mode . elispfl-ielm-mode)))

;; https://github.com/kjambunathan/fontmenu
(use-package fontmenu
  :quelpa (fontmenu :fetcher github :repo "laurencewarne/fontmenu" :upgrade t))

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
  :delight which-key-mode
  :config
  (which-key-mode))

;; https://github.com/hvesalai/emacs-scala-mode
(use-package scala-mode
  :after projectile
  :mode "\\.s\\(c\\|cala\\|bt\\)$"
  :bind (:map scala-mode-map
              ("M-k" . projectile-toggle-between-implementation-and-test)
              ("<return>" . lw-newline-smart-indent))
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
  :hook ((scala-mode . lsp-deferred)
         (scala-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t))))
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off")))
;; https://github.com/emacs-lsp/dap-mode
(use-package dap-mode
  :config
  (defun lw-dap-go-to-output-buffer (&optional no-select)
    "Go to output buffer."
    (interactive)
    (let* ((buf (dap--debug-session-output-buffer (dap--cur-session-or-die)))
           (win (display-buffer-below-selected
                 buf
                 `((side . bottom) (slot . 5) (window-height . 0.50)))))
      (with-current-buffer buf (compilation-mode 1))
      (set-window-dedicated-p win t)
      (unless no-select (select-window win))))
  (advice-add 'dap-go-to-output-buffer :override #'lw-dap-go-to-output-buffer))

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; https://github.com/ppareit/graphviz-dot-mode
(use-package graphviz-dot-mode
  :mode "\\.dot\\'"
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
          ;; doesn't work?!
          ;;(list-unicode-display-mode :select t :custom lw-shackle-get-window-cur)
          ("*Unicode Characters*" :select t :custom lw-shackle-get-window-cur)
          ;;("* Merriam-Webster.*" :regexp t :custom lw-shackle-get-window-cur)
          ;;("\*docker.*" :regexp t :select t :custom lw-shackle-get-window-cur)
          ))
  (shackle-mode 1))

(use-package openapi-yaml-mode
  :disabled t  ;; atm depends on (removed) eaf
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
   `(".*jaegertracing.*"  ;; https://www.jaegertracing.io/docs/1.6/getting-started/#all-in-one-docker-image
     ;; For the Jaeger UI open: http://localhost:16686/
     ("-d" "--name jaeger" "-e COLLECTOR_ZIPKIN_HOST_PORT=:9411" "-p 5775:5775/udp -p 6831:6831/udp -p 6832:6832/udp -p 5778:5778 -p 16686:16686 -p 14268:14268 -p 14250:14250 -p 9411:9411" . ,docker-image-run-default-args)))
  (add-to-list
   'docker-image-run-custom-args
   `(".*mysql.*"  ; https://hub.docker.com/_/mysql/
     ("--name some-mysql" "-e MYSQL_ROOT_PASSWORD=root" . ,docker-image-run-default-args))))

;; https://github.com/jcs-elpa/goto-line-preview
(use-package goto-line-preview
  :commands goto-line-preview
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
  (finito-download-server-if-not-exists #'finito-start-server-if-not-already)
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
  (setq transient-display-buffer-action '(display-buffer-below-selected)
        finito--debug t))

;; https://github.com/davazp/graphql-mode
(use-package graphql-mode
  :mode "\\.(gql|graphql)\\'"
  :bind (:map graphql-mode-map
              ("C-c C-c" . nil)))

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
  (define-key polymode-mode-map (kbd "M-n") nil))

;; https://polymode.github.io/
(use-package poly-markdown
  :hook (markdown-mode . poly-markdown-mode))

;; https://github.com/mattiase/xr
(use-package xr
  :defer t)

;; https://www.gnu.org/software/auctex/manual/auctex/index.html
(use-package tex
  :defer t
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
  :init
  (defun lw-maybe-projectile-eshell (arg)
    "Call `projectile-run-eshell' if within a project, else `eshell'.

If a prefix argument is present, run `eshell' without checking if the current
directory is part of a projectile project."
    (interactive "P")
    (require 'eshell-prompt-extras)
    ;; Don't need :after projectile
    (if (and (fboundp #'projectile-project-root) (projectile-project-root)
             (or (not (numberp (car-safe arg))) (/= (car-safe arg) 4)))
        (projectile-run-eshell)
      (eshell)))
  :bind (("C-M-t" . lw-maybe-projectile-eshell))
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
        epe-pipeline-show-time nil))

;; https://github.com/akreisher/eshell-syntax-highlighting
(use-package eshell-syntax-highlighting
  :after eshell
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

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
         (nxml-mode . aggressive-indent-mode))
  :config
  (add-hook 'emacs-startup-hook
            (lambda () (when-let ((buf (get-buffer "*scratch*")))
                         (with-current-buffer buf
                           (aggressive-indent-mode -1))))))

(use-package ts)

;; Note this is NOT yaml-mode, this package is just for yaml parsing
;; https://github.com/zkry/yaml.el/
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
(use-package font-lock-studio
  :commands font-lock-studio)

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
  :delight (whole-line-or-region-global-mode) (whole-line-or-region-local-mode)
  :config
  (whole-line-or-region-global-mode))

(use-package saws
  :if (f-exists-p "~/projects/saws.el")
  :load-path "~/projects/saws.el")

;; https://github.com/casouri/undo-hl
(use-package undo-hl
  :ensure nil
  :quelpa (undo-hl :fetcher github :repo "casouri/undo-hl")
  :delight undo-hl-mode
  :hook ((text-mode . undo-hl-mode)
         (prog-mode . undo-hl-mode)))

;; https://github.com/pythonic-emacs/blacken/blob/master/blacken.el
(use-package blacken
  :after python
  :delight blacken-mode
  :custom (blacken-only-if-project-is-blackened t)
  :hook (python-mode . blacken-mode))

;; https://github.com/wbolster/emacs-python-pytest
(use-package python-pytest
  :commands python-pytest-dispatch)

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
(use-package typescript-mode
  :mode "\\.ts\\'")

;; https://github.com/yjwen/org-reveal/
(use-package ox-reveal
  :config
  (setq org-reveal-root
        (or (-some--> (-first #'f-exists-p '("~/repos/reveal.js/"
                                             "~/projects/reveal.js/"))
              (concat "file://" (f-expand it)))
            org-reveal-root))
  (setq org-reveal-title-slide "<h2>%t</h2><h3>%a</h3><p>%d<p>"
        org-reveal-klipsify-src t))

;; https://github.com/immerrr/lua-mode
(use-package lua-mode
  :mode "\\.lua\\'")

;; https://github.com/emacsorphanage/terraform-mode
(use-package terraform-mode
  :mode "\\.tf\\'")

;; https://github.com/larstvei/Try
(use-package try
  :commands try)

;; https://github.com/agzam/mw-thesaurus.el
(use-package mw-thesaurus
  :commands (mw-thesaurus-lookup-dwim mw-thesaurus-lookup))

;; https://github.com/purcell/list-unicode-display
(use-package list-unicode-display
  :bind (("C-c u" . lw-unicode-new-window)
         :map list-unicode-display-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("q" . kill-buffer-and-window)
         ("k" . kill-buffer-and-window))
  :init
  (defun lw-unicode-new-window ()
    (interactive)
    (save-current-buffer (call-interactively #'list-unicode-display))
    (let ((display-buffer-alist '((".*" display-buffer-below-selected))))
      (select-window (display-buffer "*Unicode Characters*")))
    (mapc (lambda (w) (unless (eq w (selected-window))
                        (save-selected-window
                          (select-window w)
                          (bury-buffer))))
          (get-buffer-window-list "*Unicode Characters*")))
  :commands (list-unicode-display lw-unicode-new-window))

;; https://github.com/minad/marginalia
(use-package marginalia
  :demand t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; http://tuhdo.github.io/helm-intro.html
(use-package helm
  ;; I don't know a consult equivalent which does what helm-occur does,
  ;; consult-line only searches lines and I want to highlight occurrences
  ;; on the same line and also have the search string default to whats after
  ;; point
  :bind (("C-s" . helm-occur)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("C-," . helm-beginning-of-buffer)
         ("C-." . helm-end-of-buffer)
         ("C-k" . helm-buffer-run-kill-buffers)
         ("M-D" . helm-delete-minibuffer-contents)
         ("M-e" . helm-select-action))
  :config
  (setq helm-split-window-in-side-p t)
  ;; Stop helm-eshell-history opening a new frame
  (setq helm-show-completion-display-function #'helm-show-completion-default-display-function)
  (require 'helm-occur)
  (define-key helm-occur-map (kbd "C-s") #'helm-next-line)
  (define-key helm-occur-map (kbd "C-r") #'helm-previous-line)
  (add-hook
   'eshell-mode-hook
   (lambda ()
     (define-key eshell-mode-map (kbd "C-M-r") #'helm-eshell-history))
   99))

;; https://github.com/minad/vertico
(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group)
              ("C-l" . vertico-directory-delete-word)
              ("<return>" . vertico-directory-enter))
  :config
  (setq vertico-buffer-display-action '(display-buffer-below-selected))
  (vertico-mode)
  (vertico-buffer-mode))

;; https://github.com/minad/consult
(use-package consult
  :bind (("C-j" . lw-consult-project-buffer)
         ;; ("C-s" . consult-line)
         ("M-q" . consult-ripgrep)
         ;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ;;("M-#" . consult-register-load)
         ;;("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;;("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;; ("M-s d" . consult-find)
         ;; ("M-s D" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s m" . consult-multi-occur)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  (fset 'lw-consult-switch
        (lw-projectile-if-in-project #'consult-project-buffer
                                     #'consult-buffer))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Use consult for <Tab> in eshell and such
  (setq completion-in-region-function #'consult-completion-in-region)
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))

  (setq consult-line-start-from-top t)

  (defvar consult-projectile--source-projectile-buffer
    (list :name     "Project Buffer"
          :narrow   '(?b . "Buffer")
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :enabled  #'projectile-project-root
          :items
          (lambda ()
            (when-let (root (projectile-project-root))
              (mapcar #'buffer-name
                      (seq-filter (lambda (x)
                                    (when-let (file (buffer-file-name x))
                                      (string-prefix-p root file)))
                                  (consult--buffer-query :sort 'visibility)))))))

  (defvar consult-projectile--source-projectile-file
    (list :name     "Project File"
          :narrow   '(?f . "File")
          :category 'file
          :face     'consult-file
          :history  'file-name-history
          :action   (lambda (f) (consult--file-action (concat (projectile-acquire-root) f)))
          :enabled  #'projectile-project-root
          :items
          (lambda ()
            (projectile-project-files (projectile-acquire-root)))))

  (defvar consult-projectile--source-fallback
    (list :name     "Buffer"
          :narrow   ?b
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :enabled  (lambda () (null (projectile-project-root)))
          :items
          (lambda () (consult--buffer-query :sort 'visibility
                                            :as #'buffer-name))))  

  (setq consult-project-buffer-sources
        '(consult-projectile--source-projectile-buffer
          consult-projectile--source-projectile-file
          consult-projectile--source-fallback))
  (defun lw-consult-project-buffer ()
    (interactive)
    (consult-buffer consult-project-buffer-sources))
  (consult-customize consult-project-buffer :preview-key nil)
  (consult-customize lw-consult-project-buffer :preview-key nil)
  (consult-customize consult-buffer :preview-key nil))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; https://github.com/oantolin/embark
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :bind
  (("C-M-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h b" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; https://github.com/gagbo/consult-lsp
(use-package consult-lsp)

;; https://github.com/nim-lang/nim-mode
(use-package nim-mode
  :mode "\\.nim\\'")


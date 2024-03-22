package `(let ((base (mapconcat 'identity (split-string (replace-regexp-in-string ".*src\\(/\\(main\\|test\\)\\)?\\(/scala\\)?" "" default-directory) "/" t) ".")))
    (concat (if lw-scala-package-prefix
                (progn
                  (require 's)
                  (concat lw-scala-package-prefix
                          (mapconcat #'identity
                                     (s-split "\\." (s-chop-prefix lw-scala-package-prefix base))
                                     "\npackage ")))
              base) "\n"))`

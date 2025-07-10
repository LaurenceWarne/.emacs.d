((nil . ((fill-column . 80)))
 ;; Use eval to run stuff!
 (haskell-mode . ((eval . (highlight-regexp "^ *"))))
 (c-mode . ((c-file-style . "BSD")))
 (java-mode . ((c-file-style . "BSD")))
 ;; Specify a different ChangeLog file name for any file in the src/imported subdirectory
 ("src/imported"
  . ((nil . ((change-log-default-name . "ChangeLog.local"))))))

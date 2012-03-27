
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)

(setq migemo-use-pattern-alist t)
(setq migemo-pattern-alist-file (concat user-emacs-directory "tmp/migemo-pattern"))
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-frequent-pattern-alist-file (concat user-emacs-directory "tmp/migemo-frequent"))
(setq migemo-pattern-alist-length 2048)

(cond
 ((file-executable-p "/usr/bin/cmigemo")
  (setq migemo-command "/usr/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-coding-system 'utf-8-unix))
 (t
  (setq migemo-command "/usr/bin/migemo")
  (setq migemo-options '("-S" "migemo" "-t" "emacs"  "-i" "\a")))
  )
(load-library "migemo")
(migemo-init)

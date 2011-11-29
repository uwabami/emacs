
(set-language-environment "Japanese")
(set-language-environment-coding-systems "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

(cond
 (mac-p
  (require 'ucs-normarize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq default-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 (windows-p
  (setq file-name-coding-system 'sjis-dos)
  (setq default-file-name-coding-system 'sjis-dos)
  (setq locale-coding-system 'utf-8))
 (t
  (setq file-name-coding-system 'utf-8)
  (setq default-file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
 )

;; (auto-install-from-url "http://nijino.homelinux.net/emacs/cp5022x.el")
(require 'cp5022x)

(cd "~/")

(global-set-key (kbd "C-h")     'backward-delete-char)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-c ;")   'comment-region)
(global-set-key (kbd "C-c M-;") 'uncomment-region)
(global-set-key (kbd "C-/")     'undo)
(global-set-key (kbd "C-c M-r") 'replace-regexp)
(global-set-key (kbd "C-c r")   'replace-string)
(global-set-key [home]          'beginning-of-buffer)
(global-set-key [end]           'end-of-buffer)

(line-number-mode 0)

(column-number-mode 0)

(setq linum-format "%4d ")

(setq mode-line-frame-identification " ")
(setq-default mode-line-format
              '("-"
                mode-line-mule-info
                mode-line-modified
                mode-line-frame-identification
                mode-line-buffer-identification
                " "
                global-mode-string
                " %[("
                mode-name
                mode-line-process
                minor-mode-alist
                "%n" ")%]-"
                ;; (which-func-mode ("" which-func-format "-"))
                ;; (line-number-mode "L%l-")
                ;; (column-number-mode "C%c-")
                ;; (-3 . "%p")
                "-%-")
              )

(tool-bar-mode 0)

(set-scroll-bar-mode nil)

(menu-bar-mode -1)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t)

(setq auto-image-file-mode nil)

(global-hl-line-mode 0)

(setq next-line-add-newlines nil)
(put 'set-goal-column 'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setq transient-mark-mode t)

(show-paren-mode t)

(cond
  (window-system
    (setq frame-title-format "%b"))
  (t
    (setq frame-title-format nil))
)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-min-dir-content 1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-exclude
      '(
        "\\~$"
        "\\.elc$"
        "\\.dvi$"
        ".recentf$"
        ".howm-keys$"
        "^/var/folders/"
        "^/tmp/"
        "^/[^/:]+:"
        ))
(add-hook 'kill-emacs-query-functions 'recentf-cleanup)
(setq recentf-save-file "~/.emacs.d/tmp/recentf")

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file
      (convert-standard-filename
       (concat user-emacs-directory "tmp/emacs-places")))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq-default fill-column 72)
(setq paragraph-start '"^\\([ 　・○<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")
(setq-default auto-fill-mode nil)

(require 'ibuffer)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (add-hook 'after-save-hook
;;           'executable-make-buffer-file-executable-if-script-p)

(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))

(defun my:make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg
        (progn
          (setq arg 0)
          (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))
(defun my:buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))
(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my:make-scratch 0) nil)
                        t))))
(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら
          ;; *scratch* バッファを新しく作る.
          (function
           (lambda ()
             (unless (member "*scratch*" (my:buffer-name-list))
               (my:make-scratch 1)))))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

(when (locate-library "server")
  (require 'server)
  (eval-when-compile (require 'server))
  (when (and (functionp 'server-running-p) (not (server-running-p)))
    (server-start)))

(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-line-limit 10)
(setq time-stamp-start "$Lastupdate: ")
(setq time-stamp-format "%04y/%02m/%02d %02H:%02M:%02S")
(setq time-stamp-end "\\$")

(require 'browse-url)
;; ブラウザの設定 -> (firefox) 用
(defun browse-url-firefox
  (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  ;; URL encode any `confusing' characters in the URL.  This needs to
  ;; include at least commas; presumably also close parens.
  (while (string-match "[,)]" url)
    (setq url
          (replace-match
           (format "%%%x" (string-to-char (match-string 0 url))) t t url)))
  (let* ((process-environment (browse-url-process-environment))
         (process
          (apply 'start-process
                 (concat "mozilla-firefox " url) nil
                 "firefox"
                 (append
                  (list "-remote"
                        (concat "openurl("
                                url
                                ",new-tab)"
                                ))))))
    (set-process-sentinel process
                          `(lambda (process change)
                             (browse-url-firefox-sentinel process ,url)))))

(defun browse-url-firefox-sentinel (process url)
  "Handle a change to the process communicating with Firefox."
  (or (eq (process-exit-status process) 0)
      (let* ((process-environment (browse-url-process-environment)))
        ;; Mozilla is not running - start it
        (message "Starting Firefox...")
        (apply 'start-process (concat "mozilla-firefox " url) nil
               "firefox"
               (append browse-url-mozilla-startup-arguments (list url))))))
;;
;; (setq browse-url-generic-program
;;       (if (file-exists-p "/usr/bin/chromium")
;;           "/usr/bin/chromium"
;;         "/usr/bin/x-www-browser"))
;; (if (file-exists-p "/usr/bin/chromium")
;;     (progn
;;       (setq browse-url-browser-function 'browse-url-generic)
;;       (if (locate-library "edit-server")
;;           (progn
;;             (require 'edit-server)
;;             (setq edit-server-new-frame nil)
;;             (edit-server-start))))
(setq browse-url-browser-function 'browse-url-firefox)
(global-set-key "\C-c\C-j" 'browse-url-at-point)

(require 'epa-file)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(my:load-org-file  "init-auto-install.org")

(when window-system (my:load-org-file "init-e2wm.org"))

(my:not-locate-library elscreen "site-lisp/elscreen")
(setq elscreen-prefix-key "\C-o")
(if (eq window-system 'x)
    (setq elscreen-display-tab 6)
  (setq elscreen-display-tab nil)
  )
(setq elscreen-tab-display-kill-screen nil)
(require 'elscreen)

(when (not window-system)
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

(defface my-face-r-1 '((t (:background "gray10"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray50"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-r-1 'my-face-r-1)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     ;;("[\r]*\n" 0 my-face-r-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

(my:not-locate-library color-theme-autoloads "site-lisp/color-theme-6.6.0")
(add-to-load-path "site-lisp/color-theme-darkpastel")
(setq color-theme-load-all-themes nil)
(setq color-theme-libraries nil)
(require 'color-theme-autoloads)
(eval-after-load "color-theme-autoloads"
  '(progn
    (color-theme-initialize)
    (require 'color-theme-darkpastel)
    ))
(color-theme-darkpastel)

(when window-system (my:load-org-file "init-frame.org"))

;;  (my:load-org-file "init-migemo.org")

;;  (my:load-org-file "init-ddskk.org")

(when (locate-library "wl")
  (my:load-org-file "init-wl.org"))

;; (when (locate-library "lookup")
;;   (my:load-org-file "init-lookup-el.org"))

;; (my:load-org-file "init-muse.org")

;; (my:load-org-file "init-org.org")

;; (my:load-org-file "init-howm.org")

;; (my:load-org-file "init-programing.org")

;; (when (locate-library "rd-mode")
;;   (my:load-org-file "init-tdiary.org"))

;; (when (locate-library "tex-site")
;;   (my:load-org-file "init-auctex.org"))

;; (my:load-org-file "init-auto-complete.org")

;; (my:load-org-file "init-autoinsert.org")

;; (my:not-locate-library magit "site-lisp/magit")
;; (require 'magit)

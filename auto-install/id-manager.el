;;; id-manager.el --- id-password management

;; Copyright (C) 2009, 2010, 2011  SAKURAI Masashi
;; Time-stamp: <2011-02-21 21:32:13 sakurai>

;; Author: SAKURAI Masashi <m.sakurai atmark kiwanami.net>
;; Keywords: password, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ID-password management utility.
;; This utility manages ID-password list and generates passwords.

;; The ID-password DB is saved in the tab-separated file.  The default
;; file name of the DB `idm-database-file' is "~/.idm-db.gpg".
;; The file format is following:
;;   (name)^t(ID)^t(password)^t(Update date "YYYY/MM/DD")[^t(memo)]
;; . One can prepare an initial data or modify the data by hand or
;; the Excel.
;;
;; Implicitly, this elisp program expects that the DB file is
;; encrypted by the some GPG encryption elisp, such as EasyPG or
;; alpaca.
;;
;; Excuting the command `idm-open-list-command', you can open the
;; ID-password list buffer. Check the function `describe-bindings'.

;;; Installation:

;; To use this program, locate this file to load-path directory,
;; and add the following code to your .emacs.
;; ------------------------------
;; (require 'id-manager)
;; ------------------------------
;; If you have anything.el, bind `id-manager' to key,
;; like (global-set-key (kbd "M-7") 'id-manager).

;;; Setting example:

;; For EasyPG users:
;;
;; (autoload 'id-manager "id-manager" nil t)
;; (global-set-key (kbd "M-7") 'id-manager)                     ; anything UI
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)  ; saving password
;; (setenv "GPG_AGENT_INFO" nil)                                ; non-GUI password dialog.

;; For alpaca users:
;;
;; (autoload 'id-manager "id-manager" nil t)
;; (global-set-key (kbd "M-7") 'id-manager) ; anything UI
;; (setq idm-db-buffer-save-function ; adjustment for alpaca.el
;;       (lambda (file)
;;         (set-visited-file-name file)
;;         (alpaca-save-buffer))
;;       idm-db-buffer-password-var  ; if you are using `alpaca-cache-passphrase'.
;;         'alpaca-passphrase)

;;; Current implementation:

;; This program generates passwords by using external command:
;; `idm-gen-password-cmd'. If you have some better idea, please let me
;; know.
;;
;; I think that this program makes lazy password management more
;; securely.  But I'm not sure that this program is secure enough.
;; I'd like many people to check and advice me.

;;; Code:

(eval-when-compile (require 'cl))

(require 'widget)
(eval-when-compile (require 'wid-edit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting

(defvar idm-database-file "~/.idm-db.gpg"
  "Encripted id-password database file. The file name may
  end with '.gpg' for encryption by the GnuPG.")

(defvar idm-gen-password-cmd
  "head -c 10 < /dev/urandom | uuencode -m - | tail -n 2 |head -n 1 | head -c10")
;;  "openssl rand 32 | uuencode -m - | tail -n 2 |head -n 1 | head -c10"
;;  ...any other password generation ?

(defvar idm-copy-action
  (lambda (text) (x-select-text text))
  "Action for copying a password text into clipboard.")

(defvar idm-db-buffer-load-function
  'find-file-noselect
  "File loading function. This function has one argument FILENAME and returns a buffer,
  like `find-file-noselect'. Some decryption should work at this
  function.")

(defvar idm-db-buffer-save-function
  'write-file
  "File saving function. This function has one arguments FILENAME,
  like `write-file'. Some encryption should work at this
  function.")

(defvar idm-db-buffer-password-var nil
  "Password variable. See the text of settings for alpaca.el. ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros

(defmacro idm--aif (test-form then-form &rest else-forms)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'idm--aif 'lisp-indent-function 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Management API

(defun idm-gen-password ()
  "Generate a password."
  (let ((buf (get-buffer-create " *idm-work*")) ret)
    (call-process-shell-command
     idm-gen-password-cmd
     nil buf nil)
    (with-current-buffer buf
      (setq ret (buffer-string)))
    (kill-buffer buf)
    ret))

;; record struct
(defstruct (idm-record
            (:constructor make-idm-record-bylist
                          (name account-id password update-time
                                &optional memo)))
  name account-id password update-time memo)

(defun idm-load-db ()
  "Load the DB file `idm-database-file' and make a DB object."
  (let* ((coding-system-for-read 'utf-8)
         (tmpbuf
          (funcall idm-db-buffer-load-function
                   (expand-file-name idm-database-file)))
         db-object)
    (unwind-protect
        (let ((db (idm--make-db tmpbuf)))
          (when idm-db-buffer-password-var
            (with-current-buffer tmpbuf
              (funcall db 'file-password
                       (symbol-value idm-db-buffer-password-var))))
          db)
      (kill-buffer tmpbuf))))

(defun idm--save-db (records &optional password)
  "Save RECORDS into the DB file `idm-database-file'. This
function is called by a DB object."
  (let ((coding-system-for-write 'utf-8)
        (tmpbuf (get-buffer-create " *idm-tmp*")))
    (with-current-buffer tmpbuf
      (erase-buffer)
      (goto-char (point-min))
      (dolist (i records)
        (insert (concat (idm-record-name i) "\t"
                        (idm-record-account-id i) "\t"
                        (idm-record-password i) "\t"
                        (idm-record-update-time i)
                        (idm--aif (idm-record-memo i)
                            (concat "\t" it))
                        "\n")))
      (when password
        (set idm-db-buffer-password-var password))
      (funcall idm-db-buffer-save-function idm-database-file)
      (kill-buffer tmpbuf))))

(defun idm--make-db (tmpbuf)
  "Build a database management object from the given buffer text.
The object is a dispatch function. One can access the methods
`funcall' with the method name symbol and some method arguments."
  (lexical-let (records (db-modified nil) file-password)
    (idm--each-line
     tmpbuf
     (lambda (line)
       (let ((cols (split-string line "\t")))
         (if (or (= 4 (length cols))
                 (= 5 (length cols)))
             (push (apply 'make-idm-record-bylist cols)
                   records)))))
    (lambda (method &rest args)
      (cond
       ((eq method 'get)                      ; get record object by name
        (lexical-let ((name (car args)) ret)
          (mapc (lambda (i)
                  (if (equal name (idm-record-name i))
                      (setq ret i)))
                records)
          ret))
       ((eq method 'get-all-records) records) ; get-all-records
       ((eq method 'add-record)               ; add-record
        (progn
          (lexical-let* ((record (car args))
                         (name (idm-record-name record)))
            (setf records (loop for i in records
                                unless (equal (idm-record-name i) name)
                                collect i))
            (push record records)
            (setq db-modified t))))
       ((eq method 'delete-record-by-name)    ; delete-record-by-name
        (lexical-let ((name (car args)))
          (setf records (loop for i in records
                         unless (equal (idm-record-name i) name)
                         collect i))
          (setq db-modified t)))
       ((eq method 'set-modified)             ; set-modified
        (setq db-modified t))
       ((eq method 'save)                     ; save
        (when db-modified
          (idm--save-db records file-password)
          (setq db-modified nil)))
       ((eq method 'file-password)            ; file-password
        (setq file-password (car args)) nil)
       (t (error "Unknown method [%s]" method))))))

(defun idm--each-line (buf task)
  "Execute the function TASK with each lines in the buffer
`buf'. This function is called by `idm--make-db'."
  (with-current-buffer buf
    (goto-char (point-min))
    (unless (eobp)
      (while
          (let ((line
                 (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))
            (funcall task line)
            (forward-line 1)
            (not (eobp)))))))

(defun idm--strtime (time)
  "Translate emacs time to formatted string."
  (format-time-string "%Y/%m/%d" time))

(defun idm--parsetime (str)
  "Translate formatted string to emacs time."
  (when (string-match "\\([0-9]+\\)\\/\\([0-9]+\\)\\/\\([0-9]+\\)" str)
    (apply 'encode-time
           (let (ret)
             (dotimes (i 6)
               (push (string-to-number (match-string (+ i 1) str)) ret))
             ret))))

(defun idm--message (&rest args)
  "Show private text in the echo area without message buffer
recording."
  (let (message-log-max)
    (apply 'message args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI

(defvar idm-show-password nil
  "Display passwords switch. If this variable is non-nil, some
  functions show the password as plain text.")

(defun idm-toggle-show-password ()
  "Toggle the switch for display passwords. This function does not update views."
  (interactive)
  (setq idm-show-password (not idm-show-password)))

(defun idm-add-record-dialog (db on-ok-func)
  "Make an account record interactively and register it with DB."
  (lexical-let ((db db) (on-ok-func on-ok-func))
    (idm-edit-record-dialog
     (make-idm-record)
     (lambda (r)
       (cond
        ((funcall db 'get (idm-record-name r))
         (idm-edit-record-dialog r on-ok-func nil "Record [%s] exists!"))
        (t (funcall on-ok-func r)))))))

(defun idm-edit-record-dialog (record on-ok-func &optional password-show error-msg)
  "Pop up the edit buffer for the given record.
If the user pushes the `ok' button, the function
`idm-edit-record-dialog-commit' is called."
  (let ((before-win-num (length (window-list)))
        (main-buf (current-buffer))
        (edit-buf (idm--edit-record-dialog-buffer record on-ok-func password-show error-msg)))
    (with-current-buffer edit-buf
      (set (make-local-variable 'idm-before-win-num) before-win-num)
      (set (make-local-variable 'idm-main-buf) main-buf))
    (pop-to-buffer edit-buf)))

(defun idm--edit-record-dialog-buffer (record on-ok-func &optional password-show error-msg)
  "Return the editing buffer for the given record."
  (let ((buf (get-buffer-create "*idm-record-dialog*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (kill-all-local-variables)
      (remove-overlays)
      (widget-insert
       (format "Record: %s\n\n"
               (idm--aif (idm-record-name record) it "(new record)")))
      (when error-msg
        (widget-insert
         (let ((text (substring-no-properties error-msg)))
           (put-text-property 0 (length text) 'face 'font-lock-warning-face text)
           text))
        (widget-insert "\n\n"))
      (lexical-let
          ((record record) (on-ok-func on-ok-func) (error-msg error-msg)
           fname fid fpassword fmemo cbshow bgenerate fields)
        ;; create dialog fields
        (setq fname (widget-create
                     'editable-field
                     :size 20 :format "  Account Name: %v \n"
                     :value (or (idm-record-name record) ""))
              fid (widget-create
                   'editable-field
                   :size 20 :format "  Account ID  : %v \n"
                   :value (or (idm-record-account-id record) ""))
              fpassword (widget-create
                         'editable-field
                         :size 20 :format "  Password: %v \n"
                         :secret (and (not password-show) ?*)
                         :value (or (idm-record-password record) "")))
        (widget-insert "    (show password ")
        (setq cbshow
              (widget-create 'checkbox  :value password-show))
        (widget-insert " ) ")
        (setq bgenerate
              (widget-create 'push-button "Generate"))
        (widget-insert "\n")
        (setq fmemo (widget-create
                     'editable-field
                     :size 20
                     :format "  Memo   : %v \n"
                     :value (or (idm-record-memo record) "")))
        (setq fields
              (list 'name fname 'id fid 'password fpassword 'memo fmemo 'password-show cbshow))

        ;; OK / Cancel
        (widget-insert "\n")
        (widget-create
         'push-button
         :notify (lambda (&rest ignore)
                   (idm-edit-record-dialog-commit record fields on-ok-func))
         "Ok")
        (widget-insert " ")
        (widget-create
         'push-button
         :notify (lambda (&rest ignore)
                   (idm-edit-record-kill-buffer))
         "Cancel")
        (widget-insert "\n")

        ;; add event actions
        (widget-put cbshow
                    :notify
                    (lambda (&rest ignore)
                      (let ((current-record
                             (make-idm-record
                              :name (widget-value fname)
                              :account-id (widget-value fid)
                              :password (widget-value fpassword)
                              :memo (widget-value fmemo)))
                            (password-show (widget-value cbshow)))
                        (message "CLICK : %s" password-show)
                        (idm-edit-record-kill-buffer)
                        (idm-edit-record-dialog
                         current-record on-ok-func password-show error-msg)
                        (widget-forward 3))))
        (widget-put bgenerate
                    :notify
                    (lambda (&rest ignore)
                      (widget-value-set fpassword (idm-gen-password))
                      (widget-setup)))

        ;; setup widgets
        (use-local-map widget-keymap)
        (widget-setup)
        (goto-char (point-min))
        (widget-forward 1)))
    buf))

(defun idm-edit-record-dialog-commit (record fields on-ok-func)
  "edit-record-dialog-commit"
  (let ((name-value (widget-value (plist-get fields 'name))))
    (cond
     ((or (null name-value)
          (string-equal "" name-value))
      (idm-edit-record-kill-buffer)
      (idm-edit-record-dialog
       record on-ok-func
       (widget-value (plist-get fields 'password-show))
       "Should not be empty!"))
     (t
      (setf (idm-record-name record) name-value
            (idm-record-account-id record)
            (widget-value (plist-get fields 'id))
            (idm-record-password record)
            (widget-value (plist-get fields 'password))
            (idm-record-memo record)
            (widget-value (plist-get fields 'memo))
            (idm-record-update-time record) (idm--strtime (current-time)))
      (idm-edit-record-kill-buffer)
      (funcall on-ok-func record)))))

(defun idm-edit-record-kill-buffer ()
  "edit-record-kill-buffer"
  (interactive)
  (let ((cbuf (current-buffer))
        (win-num (length (window-list)))
        (next-win (get-buffer-window idm-main-buf)))
    (when (and (not (one-window-p))
               (> win-num idm-before-win-num))
      (delete-window))
    (kill-buffer cbuf)
    (when next-win (select-window next-win))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; id-password list buffer

(defun idm-open-list (db)
  "Open id-password list buffer."
  (lexical-let ((buf (get-buffer-create "ID-Password List"))
                (db db))
    (with-current-buffer buf
      (idm--layout-list db)
      (idm--set-list-mode db)
      )
    (set-buffer buf)))

(defun idm--put-text-property (text attr val)
  "Put a text property on the whole text."
  (put-text-property 0 (length text) attr val text) text)

(defun idm--put-record-id (text id)
  "Put the record id with the text property `idm-record-id'."
  (idm--put-text-property text 'idm-record-id id))

(defun idm--get-record-id ()
  "Get the record id on the current point."
  (get-text-property (point) 'idm-record-id))

(defun idm--layout-list (db &optional order)
  "Erase the content in the current buffer and insert record
lines. ORDER is sort key, which can be `time', `name' and `id'."
  (unless order
    (setq order 'name))
  (let* ((name-max (length "Account Name"))
         (id-max (length "ID"))
         (pw-max (length "Password"))
         (pw-mask "********")
         (pw-getter (lambda (record)
                      (if idm-show-password
                          (idm-record-password record)
                        pw-mask)))
        (cut (lambda (str) (substring str 0 (min (length str) 20))))
        numcolm (count 1)
        (line-format "%%-%ds|%%-10s |  %%-%ds | %%-%ds  :  %%-%ds   : %%s\n")
        (records (funcall db 'get-all-records)))
    (when records
      (setq numcolm (fceiling (log10 (length records))))
      (dolist (i records)
        (setq name-max (min 20 (max name-max (length (idm-record-name i))))
              id-max (min 20 (max id-max (length (idm-record-account-id i))))
              pw-max (max pw-max (length (funcall pw-getter i)))))
      (setq line-format (format line-format numcolm name-max id-max pw-max))
      (let ((buffer-read-only nil) (prev-line (line-number-at-pos)))
        (erase-buffer)
        (goto-char (point-min))
        (insert (format line-format
                        " " "Time" "Name" "ID" "Password" "Memo"))
        (insert (make-string (- (window-width) 1) ?-) "\n")
        (dolist (i (idm--sort-records records order))
          (insert
           (idm--put-record-id
            (format line-format
                    count
                    (idm-record-update-time i)
                    (funcall cut (idm-record-name i))
                    (funcall cut (idm-record-account-id i))
                    (funcall pw-getter i)
                    (idm-record-memo i))
            (idm-record-name i)))
          (incf count))
        (goto-char (point-min))
        (when (< 1 prev-line)
          (ignore-errors
            (forward-line (1- prev-line))))))))

(defun idm--sort-records (records order)
  "Sort records by the key ORDER, which can be `time', `name',
`memo' and `id'."
  (let*
      ((comparator
        (lambda (ref)
          (lexical-let ((ref ref))
            (lambda (i j)
              (let ((ii (funcall ref i))
                    (jj (funcall ref j)))
                (cond
                 ((string= ii jj) 0)
                 ((string< ii jj) -1)
                 (t 1)))))))
       (to-bool
        (lambda (f)
          (lexical-let ((f f))
            (lambda (i j)
              (< (funcall f i j) 0)))))
       (cmp-id (funcall comparator 'idm-record-account-id))
       (cmp-name (funcall comparator 'idm-record-name))
       (cmp-time (funcall comparator 'idm-record-update-time))
       (cmp-memo (funcall comparator 'idm-record-memo))
       (chain
        (lambda (a b)
          (lexical-let ((a a) (b b))
            (lambda (i j)
              (let ((v (funcall a i j)))
                (if (= 0 v)
                    (funcall b i j)
                  v)))))))
  (sort
   (loop for i in records collect i) ; copy-list
   (cond
    ((eq order 'id)   ; id -> id, name
     (funcall to-bool (funcall chain cmp-id cmp-name)))
    ((eq order 'name) ; name -> name
     (funcall to-bool cmp-name))
    ((eq order 'time) ; time -> time, name
     (funcall to-bool (funcall chain cmp-time cmp-name)))
    ((eq order 'memo) ; memo -> time, name
     (funcall to-bool (funcall chain cmp-memo cmp-name)))
    (t  ; default
     (funcall to-bool cmp-name))))))

(defvar idm-list-mode-map nil
  "Keymap for `idm-list-mode'.")
(setq idm-list-mode-map nil) ; for debug
(unless idm-list-mode-map
  (setq idm-list-mode-map (make-sparse-keymap))
  (mapc (lambda (i)
          (define-key idm-list-mode-map (car i) (cdr i)))
        `(("q" . idm-list-mode-quit)
          ("Q" . idm-list-mode-quit-without-save)

          ("n" . next-line)
          ("p" . previous-line)
          ("j" . next-line)
          ("k" . previous-line)

          ("d" . idm-list-mode-delete)
          ("-" . idm-list-mode-delete)
          ("e" . idm-list-mode-edit-dialog)
          ("m" . idm-list-mode-edit-dialog)
          ("a" . idm-list-mode-add)
          ("+" . idm-list-mode-add)

          ("u" . idm-list-mode-reload)
          ("r" . idm-list-mode-reload)

          ("T" . idm-list-mode-sortby-time)
          ("N" . idm-list-mode-sortby-name)
          ("I" . idm-list-mode-sortby-id)
          ("M" . idm-list-mode-sortby-memo)

          ("S" . idm-list-mode-toggle-show-password)
          ("s" . idm-list-mode-show-password)
          ([return] . idm-list-mode-copy)
          )))

(defun idm-list-mode-copy ()
  (interactive)
  (idm--aif (idm--get-record-id)
      (let ((record (funcall idm-db 'get it)))
        (when record
          (message (concat "Copied the password for the account ID: "
                           (idm-record-account-id record)))
          (funcall idm-copy-action (idm-record-password record))))))

(defun idm-list-mode-sortby-id ()
  (interactive)
  (idm--layout-list idm-db 'id))

(defun idm-list-mode-sortby-name ()
  (interactive)
  (idm--layout-list idm-db 'name))

(defun idm-list-mode-sortby-time ()
  (interactive)
  (idm--layout-list idm-db 'time))

(defun idm-list-mode-sortby-memo ()
  (interactive)
  (idm--layout-list idm-db 'memo))

(defun idm-list-mode-reload ()
  "Reload the id-password database file."
  (interactive)
  (setq idm-db (idm-load-db))
  (idm--layout-list idm-db))

(defun idm-list-mode-toggle-show-password ()
  "Toggle whether to show passwords."
  (interactive)
  (idm-toggle-show-password)
  (idm--layout-list idm-db))

(defun idm-list-mode-show-password ()
  "Show password of the selected record."
  (interactive)
  (idm--aif (idm--get-record-id)
      (let ((record (funcall idm-db 'get it)))
        (if record
            (idm--message
             (concat
              "ID: " (idm-record-account-id record)
              " / PW: "(idm-record-password record)))))))

(defun idm--set-list-mode (db)
  "Set up major mode for id-password list mode."
  (kill-all-local-variables)
  (make-local-variable 'idm-db)
  (setq idm-db db)

  (setq truncate-lines t)
  (use-local-map idm-list-mode-map)
  (setq major-mode 'idm-list-mode
        mode-name "ID-Password List")
  (hl-line-mode 1))

(defun idm-list-mode-quit ()
  "Save the DB and kill buffer."
  (interactive)
  (funcall idm-db 'save)
  (kill-buffer (current-buffer)))

(defun idm-list-mode-quit-without-save ()
  "Kill buffer without saving the DB."
  (interactive)
  (kill-buffer (current-buffer)))

(defun idm-list-mode-delete ()
  "Delete a selected record from the DB. After deleting, update
the list buffer."
  (interactive)
  (idm--aif (idm--get-record-id)
      (progn
        (when (y-or-n-p (format "Delete this record[%s] ?" it))
          (funcall idm-db 'delete-record-by-name it)
          (idm--layout-list idm-db)))))

(defun idm-list-mode-add ()
  "Add a new record. After adding, update the list buffer."
  (interactive)
  (lexical-let ((db idm-db)
                (curbuf (current-buffer)))
    (idm-add-record-dialog db
     (lambda (r)
       (with-current-buffer curbuf
         (funcall db 'add-record r)
         (idm--layout-list db))))))

(defun idm-list-mode-edit-dialog ()
  "Edit the selected record. After editting, update the list
buffer."
  (interactive)
  (idm--aif (idm--get-record-id)
      (let ((record (funcall idm-db 'get it)))
        (if record
            (lexical-let ((db idm-db) (prev record)
                          (curbuf (current-buffer)))
              (idm-edit-record-dialog
               record
               (lambda (r)
                 (with-current-buffer curbuf
                   (funcall db 'delete-record-by-name (idm-record-name prev))
                   (funcall db 'add-record r)
                   (idm--layout-list db)))))))))

(defun idm-open-list-command (&optional db)
  "Load the id-password DB and open a list buffer."
  (interactive)
  (unless db
    (setq db (idm-load-db)))
  (switch-to-buffer (idm-open-list db)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anything UI

(defun idm--anything-add-dialog (db)
  "Add a new record by the anything interface."
  (interactive)
  (lexical-let ((db db))
    (idm-add-record-dialog db
     (lambda (r)
       (funcall db 'add-record r)
       (funcall db 'save)
       (when (eq major-mode 'idm-list-mode)
         (idm--layout-list db))))))

(defun idm--anything-edit-dialog (db record)
  "Edit a record selected by the anything interface."
  (interactive)
  (lexical-let ((db db) (prev record))
    (idm-edit-record-dialog
     record
     (lambda (r)
       (funcall db 'delete-record-by-name (idm-record-name prev))
       (funcall db 'add-record r)
       (funcall db 'save)
       (when (eq major-mode 'idm-list-mode)
         (idm--layout-list db))))))

(defun idm-anything-command ()
  "Anything interface for id-manager."
  (interactive)
  (let* ((db (idm-load-db))
         (source-commands
          `((name . "Global Command : ")
            (candidates
             . (("Add a record" .
                 (lambda ()
                   (idm--anything-add-dialog db)))
                ("Show all records" .
                 (lambda ()
                   (idm-open-list-command db)))))
            (action . (("Execute" . (lambda (i) (funcall i)))))))
         (source-records
          '((name . "Accounts : ")
            (candidates
             . (lambda ()
                 (mapcar
                  (lambda (record)
                    (cons (concat
                           (idm-record-name record)
                           " (" (idm-record-account-id record) ") "
                           "   " (idm-record-memo record))
                          record))
                  (funcall db 'get-all-records))))
            (action
             . (("Copy password"
                 . (lambda (record)
                     (message (concat "Copied the password for the account ID: "
                                      (idm-record-account-id record)))
                     (funcall idm-copy-action (idm-record-password record))))
                ("Show ID / Password"
                 . (lambda (record)
                     (idm--message
                      (concat
                       "ID: " (idm-record-account-id record)
                       " / PW: "(idm-record-password record)))))
                ("Edit fields"
                 . (lambda (record)
                     (idm--anything-edit-dialog db record)))
                )))
          ))
    (anything
     '(source-commands source-records)
     nil "ID-Password Management : " nil nil)))

(defalias 'id-manager 'idm-open-list-command)

(eval-after-load "anything"
  '(defalias 'id-manager 'idm-anything-command))

(provide 'id-manager)
;;; id-manager.el ends here

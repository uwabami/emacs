;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;; simple-tdiary-mode.el
;;
;; Copyright(C) Youhei SASAKI All rights reserved.
;; $Lastupdate: 2011/12/20 11:51:46$
;;
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Code:
;;
;;= simple-tdiary-mode.el について
;;
;; このパッケージは「ただダラ」を Emacs から使うようにして tDiaryを更新す
;; るためのメジャーモードである simple-tdiary-mode を提供します.
;;
;; 元ネタは simple-tdiary-mode です. 多くの関数などを simple-tdiary-mode
;; からインスパイアしています. また, 肝心の日記の更新部分は tw.rb に任せっ
;; きりです(group や id の切替を考慮していない分, 改悪なのかもしれません).
;;
;; インストールや設定などの詳細については以下をご覧下さい.
;; http://www.gfd-dennou.org/member/uwabami/software/tdiary/

;;;
;; Version
;;;
(defconst simple-tdiary-version "0.0.1"
  "version of simple-tdiary-mode")
(defun simple-tdiary-version ()
  "display simple-tdiary-version"
  (interactive)
  (let ((version-string
         (format "simple-tdiary-mode-v%s" simple-tdiary-version)))
    (if (interactive-p)
        (message "%s" version-string)
      version-string)))
;;;
;; Customize variables
;;;
(defvar simple-tdiary-bin "tw.rb"
  "set PATH for tw.rb")

(defvar simple-tdiary-root "~/.tdiary"
  "set location of diary files")

(defvar simple-tdiary-username nil
  "set diary's username")

(defvar simple-tdiary-time-offset nil
  "*日付を計算する際に用いるオフセット.
6 に設定すると午前 6 時まで前日の日付として扱われる")

;; ただダラにわたすオプション
(defvar simple-tdiary-option-useragent (simple-tdiary-version)
  "*ただダラのユーザエージェントオプションを指定する.
実行時に, -a オプションとして使われる. ")

(defvar simple-tdiary-option-debug-flag nil
  "*ただダラを, デバッグモードで実行するか否かを指定するフラグ.

ただダラ実行時に, -d オプションとしてわたされ, また,その場合, 実行
結果をバッファに表示する.

デバッグモードをオン/ オフするには,
simple-tdiary-toggle-debug-mode を実行する. ")

(defvar simple-tdiary-option-timeout 30
  "*ただダラのタイムアウトを指定する.

実行時に, -T オプションとして使われる. ")

(defvar simple-tdiary-process-buffer-name "*SimpletDiary*"
  "*ただダラを実行するプロセスのバッファ名. ")

;; キーバインド
(setq simple-tdiary-mode-map (make-keymap))

(define-key simple-tdiary-mode-map (kbd "C-c C-v") 'simple-tdiary-version)
(define-key simple-tdiary-mode-map (kbd "C-c C-p") 'simple-tdiary-submit)
(define-key simple-tdiary-mode-map (kbd "C-c C-c") 'simple-tdiary-trivial-submit)
(define-key simple-tdiary-mode-map (kbd "C-c C-n") 'simple-tdiary-find-diary-for)
(define-key simple-tdiary-mode-map (kbd "C-c C-b") 'simple-tdiary-go-back)
(define-key simple-tdiary-mode-map (kbd "C-c C-f") 'simple-tdiary-go-forward)
(define-key simple-tdiary-mode-map (kbd "C-c C-d") 'simple-tdiary-toggle-debug-mode)
(define-key simple-tdiary-mode-map (kbd "C-c C-q") 'simple-tdiary-exit)
(define-key simple-tdiary-mode-map (kbd "C-c C-e") 'simple-tdiary-exit)

;; フック
(defvar simple-tdiary-mode-hook nil
  "simple-tdiary-mode 開始時のフック. ")
(defvar simple-tdiary-before-submit-hook nil
  "日記を投稿する直前のフック")
(defvar simple-tdiary-after-submit-hook nil
  "日記を投稿した直後のフック")

;; フォントロック

(defvar simple-tdiary-font-lock-keywords nil)

;;;; * 実装

(eval-when-compile
  (require 'cl)
  (require 'derived)
  (require 'font-lock)
  (require 'rd-mode)
  )

(defconst simple-tdiary-filename-regex
   "\\([^/]+\\)?/?\\([0-9][0-9][0-9][0-9]\\)-\\([01][0-9]\\)-\\([0-3][0-9]\\)\.txt"
  "日記ファイルの正規表現. マッチした場合, 以下のインデックスによりファイル情報を取得できる.

  0. マッチした全体
  1. 年 (YYYY)
  2. 月 (MM)
  3. 日 (DD)")

;; simple-tdiary-mode を, rd-mode の派生モードとして定義する.
(define-derived-mode simple-tdiary-mode rd-mode "Simple tDiary"
  "ただダラを, Emacs から利用するためのインタフェイスを提供するモード."

  ;; 現在開いているバッファの情報
  (make-local-variable 'simple-tdiary-local-current-buffer-info)
  (make-local-variable 'simple-tdiary-local-current-buffer-year)
  (make-local-variable 'simple-tdiary-local-current-buffer-month)
  (make-local-variable 'simple-tdiary-local-current-buffer-day)

  (if (string-match simple-tdiary-filename-regex (buffer-file-name))
      (progn
        (setq simple-tdiary-local-current-buffer-info
              (match-string 0 (buffer-file-name)))
        (setq simple-tdiary-local-current-buffer-year
              (match-string 1 (buffer-file-name)))
        (setq simple-tdiary-local-current-buffer-month
              (match-string 2 (buffer-file-name)))
        (setq simple-tdiary-local-current-buffer-day
              (match-string 3 (buffer-file-name)))
        (simple-tdiary-update-modeline))
    (error "Current buffer isn't related to tDiary Writer data file"))
  (font-lock-mode 1)
  (use-local-map simple-tdiary-mode-map)
  (run-hooks 'simple-tdiary-mode-hook))

;; ただダラデータに simple-tdiary-mode を適用する
;;
;; - ~/.tdiary/YYYY-MM-DD.txt
;;
;; というファイルを開いたら, simple-tdiary-mode にする
(add-to-list 'auto-mode-alist
             (cons simple-tdiary-filename-regex 'simple-tdiary-mode))

;;;; * コマンド

(defun simple-tdiary-setup ()
  (interactive)
  "ディレクトリ配置をセットアップする. "
  (and
   (simple-tdiary-setup-check-tdiary-writer-bin-exists-p)
   (simple-tdiary-setup-create-initial-directory)
   ))

(defun simple-tdiary-setup-check-tdiary-writer-bin-exists-p ()
  (if (file-executable-p simple-tdiary-bin)
      t
    (progn
      (if (y-or-n-p
           (format
            "`tDiary Writer' not found in %s. Are you sure to continue setup? "
            simple-tdiary-bin))
          t
        (progn
          (when (y-or-n-p
                 "Open the documentation of simple-tdiary-mode in your browser? ")
            (browse-url "http://www.gfd-dennou.org/member/uwabami/software/tdiary/"))
          (message "You must download and install `tDiary Writer' first")
          nil)))))

(defun simple-tdiary-setup-create-initial-directory ()
  (simple-tdiary-setup-create-directory-and-file
   (expand-file-name
    (format "%s/config.txt" simple-tdiary-root))))

(defun simple-tdiary-setup-create-directory-and-file (filename)
  "Set up a directory and file.
Note: This function create  intermediate directories as required."
  (let
      ((dirname (file-name-directory filename)))
    (unless (file-exists-p filename)
      (unless (file-directory-p dirname)
        (make-directory dirname 'parents))
      (append-to-file 1 1 filename))))

(defun simple-tdiary-read-string-and-match-check (prompt regex
                                                         &optional errmsg)
  "Read a string from the minibuffer, prompting with string prompt,
and Cheking input value. If non-nil, third args, you can set
error message.

NOTE: Please refer to `format' for the format of the error
message."
  (let
      ((input nil)
       (errmsg (or errmsg
                   "Your input is invalid...")))
    (while
        (and
         (setq input (read-string prompt))
         (not (string-match regex input)))
      (message (format errmsg input))
      (sleep-for 1))
    input))

(defun simple-tdiary ()
  "実行日現在の日付のファイルを開く. "
  (interactive)
  (find-file
   (concat
    simple-tdiary-root "/"
    (simple-tdiary-internal-make-diary-file-string 0))))

(defun simple-tdiary-submit ()
  "tDiary に投稿する. "
  (interactive)
  (simple-tdiary-internal-do-submit))

(defun simple-tdiary-trivial-submit ()
  "tDiary に「ちょっとした更新」で投稿する. "
  (interactive)
  (simple-tdiary-internal-do-submit))

(defun simple-tdiary-find-diary-for (date)
  "指定された日付の日記バッファを表示する. "
  (interactive "sDate (YYYY-MM-DD): ")
  (if (equal major-mode 'simple-tdiary-mode)
      (if (string-match "^[0-9][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]$" date)
          (find-file
           (concat (file-name-directory (buffer-file-name))
                   (concat date ".txt")))
        (error "Invalid date"))
    (error "Current major mode isn't simple-tdiary-mode")))

(defun simple-tdiary-go-forward (&optional i)
  "前の日付へ移動する. 前置引数が渡された場合は, その数だけ後の日付に移動する. "
  (interactive "p")
  (if (not i)
      (simple-tdiary-internal-go-for 1)
    (simple-tdiary-internal-go-for i)))

(defun simple-tdiary-go-back (&optional i)
  "次の日付へ移動する. 前置引数が渡された場合は, その数だけ前の日付に移動する. "
  (interactive "p")
  (if (not i)
      (simple-tdiary-internal-go-for -1)
    (simple-tdiary-internal-go-for (- i))))

(defun simple-tdiary-toggle-debug-mode ()
  "デバッグモードをオン/ オフする. "
  (interactive)
  (if simple-tdiary-option-debug-flag
      (progn
        (setq simple-tdiary-option-debug-flag nil)
        (message "Debug mode off"))
    (progn
      (setq simple-tdiary-option-debug-flag t)
      (message "Debug mode on"))))

(defun simple-tdiary-exit ()
  "simple-tdiary-mode の適用されているバッファを全て削除する. "
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and
           (buffer-file-name buffer)
           (string-match simple-tdiary-filename-regex (buffer-file-name buffer)))
      (when (buffer-modified-p buffer)
        (progn
          (save-current-buffer
            (set-buffer buffer)
            (save-buffer))))
      (kill-buffer buffer)))
  (message "simple-tdiary-mode has been exited"))

;;;; * 内部関数

(defun simple-tdiary-internal-make-diary-file-string (i &optional date)
  "date が指定されていない場合は, 実行日現在の日付を起点にした日記ファイル名を生成する.

   0: 今日
   1: 明日
  -1: 昨日

指定されている場合は, その日付を起点にした日記ファイル名を生成する. "
  (apply (lambda (s min h d mon y &rest rest)
           (format-time-string "%Y-%m-%d.txt"
                               (encode-time s min h (+ d i) mon y)))
         (if date
             (append '(0 0 0) date)
           (apply (lambda (s min h d mon y &rest rest)
                    (list s min (- h (or simple-tdiary-time-offset 0)) d mon y))
                  (decode-time)))))

(defun simple-tdiary-internal-go-for (i)
  "引数の数だけ前後の日付のファイ名バッファへ移動する. "
  (find-file
   (concat
    (file-name-directory (buffer-file-name))
    (simple-tdiary-internal-make-diary-file-string
     i
     (list (string-to-number simple-tdiary-local-current-buffer-day)
           (string-to-number simple-tdiary-local-current-buffer-month)
           (string-to-number simple-tdiary-local-current-buffer-year))))))

(defun simple-tdiary-internal-list-directories (dir)
  "dir 下にあるディレクトリをリストにして返す. "
  (let ((dir-list nil))
    (dolist (file (directory-files dir t "^[^\.]") dir-list)
      (if (file-directory-p file)
          (progn
            (string-match "\\([^/]+\\)/?$" file)
            (setq dir-list (cons (match-string 1 file) dir-list)))))))

(defun simple-tdiary-internal-do-submit (&optional flag)
  "tDiary へ日記を投稿する. "
  (let ((max-mini-window-height 10)  ; tw.rb が表示するメッセージを,
                                     ; echo エリアに表示させるため.
        (thisdir (file-name-directory (buffer-file-name))))
    (run-hooks 'simple-tdiary-before-submit-hook)
    (when (buffer-modified-p)
      (save-buffer))
    (message "%s" "Now posting...")
    (let* ((buffer (get-buffer-create simple-tdiary-process-buffer-name))
           (proc (get-buffer-process buffer)))
      (if (and
           proc
           (eq (process-status proc) 'run))
          (if (yes-or-no-p (format "A %s process is running; kill it?"
                                   (process-name proc)))
              (progn
                (interrupt-process proc)
                (sit-for 1)
                (delete-process proc))
            (error nil)))
      (with-current-buffer buffer
        (progn
          (erase-buffer)
          (buffer-disable-undo (current-buffer))
          (setq default-directory thisdir)))
      (make-comint-in-buffer
       "simple-tdiary-submit" buffer shell-file-name nil
       shell-command-switch (simple-tdiary-internal-build-command flag))
      (set-process-sentinel
       (get-buffer-process buffer)
       '(lambda (process signal)
          (if (string= signal "finished\n")
              (let ((max-mini-window-height 10))
                (display-message-or-buffer (process-buffer process))
                (run-hooks 'simple-tdiary-after-submit-hook))))))))

(defun simple-tdiary-internal-build-command (flag)
  "実行可能なコマンド文字列を作成する. "
  (let ((flag-list (list flag)))
    (if simple-tdiary-option-debug-flag  (setq flag-list (cons "-d" flag-list)))
    (simple-tdiary-internal-join
     " "
     (cons simple-tdiary-bin
           (append (simple-tdiary-internal-build-option-list-from-alist) flag-list)))))

(defun simple-tdiary-internal-build-option-list-from-alist ()
  "引数を取るオプションのリストを作成する. "
  (let ((opts nil))
    (dolist (pair
             `(("-u" . ,simple-tdiary-username)
               ("-a" . ,simple-tdiary-option-useragent)
               ("-T" . ,(format "%s" simple-tdiary-option-timeout)))
             opts)
      (if (cdr pair)
           (setq opts (append opts (list (car pair) (cdr pair))))))))

(defun simple-tdiary-internal-join (sep list)
  "車輪の再発明なんだろうけど, 見つからなかったので join 実装"
  (if (<= (length list) 1)
      (car list)
    (concat (car list) sep (simple-tdiary-internal-join sep (cdr list)))))

(defun simple-tdiary-update-modeline ()
  "モードラインの表示を更新する"
  (let (setq mode-name (format "Simple tDiary"))
    (force-mode-line-update)))

(provide 'simple-tdiary-mode)


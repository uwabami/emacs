;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;;
;; Copyright(C) Youhei SASAKI All rights reserved.
;; $Lastupdate: 2013/01/01 02:23:11$
;;
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; License: GPL-3+
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
;;; Comment:
;;
;;  org-mode に含まれる ob-tangle を改変しているので GPL-3+ で.
;;
;;; Code:
;; -----------------------------------------------------------
;;; byte-compile 関連
;;
;; 必要になることが多いので cl だけは読み込んでおく
;;
(require 'cl)
;;
;; 警告, Compile-Log の非表示
;;
(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(let ((win (get-buffer-window "*Compile-Log*")))
  (when win (delete-window win)))
;; -----------------------------------------------------------
;;; 自己紹介 -> 名前とメールアドレスの設定
;;
(setq user-full-name "Youhei SASAKI")
(setq user-mail-address "uwabami@gfd-dennou.org")
;; -----------------------------------------------------------
;;; Emacs の種類/バージョンを判別するための変数を定義
;;
;; 元々は以下のURLにあった関数. 必要な物だけ抜粋
;; @see http://github.com/elim/dotemacs/blob/master/init.el
;;
(defvar oldemacs-p (<= emacs-major-version 22)) ; 22 以下
(defvar emacs23-p (<= emacs-major-version 23))  ; 23 以下
(defvar emacs24-p (>= emacs-major-version 24))  ; 24 以上
(defvar darwin-p (eq system-type 'darwin))      ; Mac OS X 用
(defvar nt-p (eq system-type 'windows-nt))      ; Windows 用
;; -----------------------------------------------------------
;;; ディレクトリ構成を決めるための変数
;;
;; Emacs 22 以下用に user-emacs-directory を定義する.
;; 他にも以下の変数を定義
;; - my:user-emacs-config-directory    → ~/.emacs.d/config
;; - my:user-emacs-temporary-directory → ~/.emacs.d/tmp
;; - my:user-emacs-etc-directory       → ~/.emacs.d/etc
;; - my:user-emacs-share-directory     → ~/.emacs.d/share
;;
(when oldemacs-p
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))
(defconst my:user-emacs-config-directory
  (expand-file-name (concat user-emacs-directory "config/")))
(defconst my:user-emacs-temporary-directory
  (expand-file-name (concat user-emacs-directory "tmp/")))
(defconst my:user-emacs-etc-directory
  (expand-file-name (concat user-emacs-directory "etc/")))
(defconst my:user-emacs-share-directory
  (expand-file-name (concat user-emacs-directory "share/")))
(defconst my:user-emacs-template-directory
  (expand-file-name (concat user-emacs-directory "template/")))
;; -----------------------------------------------------------
;;; load-path 追加用の関数の定義
;;
;; 最後に add したものが先頭にくるようになっている. 読み込みたくないファ
;; イルは, 先頭に "." や "_" をつけると良い.
;;
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-loadpath)
            (normal-top-level-add-subdirs-to-load-path))))))
;; -----------------------------------------------------------
;;; load-path の設定
;;
;; load-path の優先順位が気になる場合には
;;      M-x list-load-path-shadows
;; で確認する.
;;
(add-to-load-path
 "config"                  ; 分割した設定群の置き場所.
 "site-lisp/org-mode/lisp" ; org-mode (Git HEAD)
 "el-get/el-get"           ; el-get で install した物.
 "local"                   ; 自作の emacs-lisp とか.
 )
;; -----------------------------------------------------------
;;; el-get の install
;;
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(add-to-list 'el-get-recipe-path
             (expand-file-name (concat user-emacs-directory "recipes")))
(setq el-get-github-default-url-type 'https)
;; -----------------------------------------------------------
;;; bundle - an el-get wrapper
;;
;; @oarat による素敵 emacs-lisp を el-get の wrapper として使う
;;
(add-to-list 'el-get-sources
             '(:name bundle
                     :url "http://gist.github.com/raw/4414297/bundle.el"
                     :type http
                     :features (bundle)))
(el-get 'sync 'bundle)
;; -----------------------------------------------------------
;;; org-babel
;;
;; Emacs の設定はorg-mode で記述する.
;;
;; @see Emacsの設定ファイルをorgで書く:
;;      http://uwabami.junkhub.org/log/20111213.html#p01
;;
(require 'org)
;; -----------------------------------------------------------
;;; ob-tangle より自分用に幾つか関数を設定
;;
;; my:org-babel-tangle-and-compile-file
;; 指定された org ファイルから emacs-lisp を export してbyte-compile
;; する. Make から呼ぶ事も想定しているのでこの段階では load はしない.
;;
(defun my:org-babel-tangle-and-compile-file (file)
  "export emacs-lisp and byte-compile from org files (not load).
   originally ob-tangle.el"
  (interactive "fFile to load: ")
  (flet ((age (file)
              (float-time
               (time-subtract (current-time)
                              (nth 5 (or (file-attributes (file-truename file))
                                         (file-attributes file)))))))
    (let* ((base-name (file-name-sans-extension file))
           (exported-file (concat base-name ".el"))
           (compiled-file (concat base-name ".elc")))
      ;; tangle if the org-mode file is newer than the elisp file
      (unless (and (file-exists-p compiled-file)
                   (> (age file) (age compiled-file)))
        (org-babel-tangle-file file exported-file "emacs-lisp")
        (byte-compile-file exported-file)))))
;; -----------------------------------------------------------
;;; my:org-babel-load-file
;;
;; my:org-babel-tangle-and-comile-file してから load する
;;
(defun my:org-babel-load-file (file)
  "load after byte-compile"
  (interactive "fFile to load: ")
  (my:org-babel-tangle-and-compile-file file)
  (load (file-name-sans-extension file)))
;; -----------------------------------------------------------
;;; my:org-load-file
;;
;; my:org-babel-load-file の際にディレクトリ名を
;; ~/.emacs.d/config/ に決め打ち
;;
(defun my:load-org-file (file)
  "my:user-emacs-config-directory 以下から my:org-babel-load-file"
  (my:org-babel-load-file
   (expand-file-name file my:user-emacs-config-directory)))
;; -----------------------------------------------------------
;;; 実際に設定を読み込む.
;;
(my:load-org-file "index.org")
;; -----------------------------------------------------------
;;; calculate bootup time/ スピード狂に捧ぐ.
;;
;; 目標: 3000ms 圏内
;;
(unless oldemacs-p
  (defun message-startup-time ()
    (message
     "Emacs loaded in %dms"
     (/ (- (+ (third after-init-time) (* 1000000 (second after-init-time)))
           (+ (third before-init-time) (* 1000000 (second before-init-time))))
        1000)))
  (add-hook 'after-init-hook 'message-startup-time))
;;; init.el ends here

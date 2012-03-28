;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;; init.el
;;
;; Copyright(C) Youhei SASAKI All rights reserved.
;; $Lastupdate: 2012/03/29 01:17:52$
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

;; 必要になることが多いので cl だけは読み込んでおく
;;
(eval-when-compile (require 'cl))
;;
;;; 自己紹介 -> 名前とメールアドレスの設定
;;
(setq user-full-name "Youhei SASAKI")
(setq user-mail-address "uwabami@gfd-dennou.org")
;;
;;; Emacs の種類/バージョンを判別するための変数を定義
;;
;; 元々は以下のURLにあった関数. 必要な物だけ抜粋
;; @see http://github.com/elim/dotemacs/blob/master/init.el
;;
;; Emacs のバージョン判定. ELPAのためにEmacs23 まで判定
;;
(defvar oldemacs-p (<= emacs-major-version 22))
(defvar emacs23-p (= emacs-major-version 23))
;;
;; Mac の Emacs flavor の判定 <-- darwin しか考えない
;;
(defvar darwin-p (eq system-type 'darwin))
;;
;; Linux の判定 <-- BSD とか使うようになったらどうするかな?
;;
;; (defvar linux-p (eq system-type 'gnu/linux))
;;
;; Windows用 <-- NT Emacs のことだけしか考えない
;;
(defvar nt-p (eq system-type 'windows-nt))
;;
;;; load-path 追加用の変数/関数
;;
;; @see http://sakito.jp/emacs/emacs23.html
;;
;; Emacs 22 以下用に user-emacs-directory を定義する.
;;
(when oldemacs-p
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))
;;
;;; load-path 追加用の関数の定義
;;
;; - 読み込みたくないファイルは, 先頭に "." や "_" をつけると良い
;; - 最後に add したものが先頭にくる
;;
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-loadpath)
            (normal-top-level-add-subdirs-to-load-path))))))
;;
;;; load-path の設定
;;
;; load-path の優先順位が気になる場合には
;;      M-x list-load-path-shadows
;; で確認する.
;;
(add-to-load-path
 ;; 分割した設定群の置き場所.
 "config"
 ;; 自作の小物など
 "local-lisp"
 ;; auto-install で install したファイル. auto-install 自体の設定は
 ;; ~/.emacs.d/config/auto-install.org 参照
 "auto-install"
 ;; org-mode は常に最新版(Git HEAD)を load する. git の submodule で
 ;; org-mode の HEAD を持って来ること.
 "site-lisp/org-mode/lisp"
 )
;;
;;; 良く使う macro の定義
;;
;; 今のところ以下を定義
;; - my:not-locate-library
;;   (not (locate-library "foobar") -> (add-to-load-path "foobar")
;;
(defmacro my:not-locate-library (lib &rest list)
  `(when (not (locate-library ,(symbol-name lib)))
     (add-to-load-path ,@list)
     (eval-when-compile
       (add-to-load-path ,@list))))
;;
;;; org-babel
;;
;; Emacs の設定はorg-mode で記述する.
;;
;; @see Emacsの設定ファイルをorgで書く:
;;      http://uwabami.junkhub.org/log/20111213.html#p01
;;
(require 'org-install)
;;
;;; ob-tangle より自分用に幾つか関数を設定
;;
;; my:org-babel-tangle-and-compile-file
;; - 指定された org ファイルから emacs-lisp を export してbyte-compile
;;   する. Make から呼ぶ事も想定しているのでこの段階では load はしない.
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
;;
;; my:org-babel-load-file
;; - my:org-babel-tangle-and-comile-file してから load する
;;
(defun my:org-babel-load-file (file)
  "load after byte-compile"
  (interactive "fFile to load: ")
  (my:org-babel-tangle-and-compile-file file)
  (load (file-name-sans-extension file)))
;;
;; my:org-load-file
;; - my:org-babel-load-file の際にディレクトリ名を
;;   ~/.emacs.d/config/ に決め打ち
;;
(defvar my:org-settings-dir (concat user-emacs-directory "config/"))
(defun my:load-org-file (file)
  "my:org-settings-dir 以下から my:org-babel-load-file"
  (my:org-babel-load-file (expand-file-name file my:org-settings-dir)))
;;
;; 実際に読み込む.
;;
(my:load-org-file "init.org")
;;
;; Emacs Deamon 用に color-theme と fontset を読み込むためだけの関数の追加
;; (defun my:load-theme ()
;;   (interactive)
;;   (my:load-org-file "colorize.org"))
;;
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
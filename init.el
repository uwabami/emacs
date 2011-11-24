;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;; init.el
;;
;; Copyright(C) Youhei SASAKI All rights reserved.
;; $Lastupdate: 2011/11/24 19:44:23$
;;
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Code:
;; -----------------------------------------------------------
(eval-when-compile (require 'cl))
;; -----------------------------------------------------------
;; 名前とメールアドレス
(setq user-full-name "Youhei SASAKI")
(setq user-mail-address "uwabami@gfd-dennou.org")
;; -----------------------------------------------------------
;;; Emacs の種類/バージョンを判別するための変数を定義
;;
;; @see http://github.com/elim/dotemacs/blob/master/init.el
;;
(defun x->bool (elt) (not (not elt)))
;; Emacs のバージョン
(defvar oldemacs-p (< emacs-major-version 22))
(defvar emacs22-p (equal emacs-major-version 22))
(defvar emacs23-p (equal emacs-major-version 23))
;; Mac の Emacs flavor の判定
(defvar darwin-p (eq system-type 'darwin))
(defvar ns-p (featurep 'ns))
(defvar carbon-p (and (eq window-system 'mac) emacs22-p))
(defvar mac-p (and (eq window-system 'mac) emacs23-p))
;; Linux の判定 <-- BSD とかどうするかな
(defvar linux-p (eq system-type 'gnu/linux))
;; Windows の Emacs flavor の判定
(defvar colinux-p (when linux-p
                    (let ((file "/proc/modules"))
                      (and
                       (file-readable-p file)
                       (x->bool
                        (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (re-search-forward "^cofuse\.+" nil t)))))))
(defvar cygwin-p (eq system-type 'cygwin))
(defvar nt-p (eq system-type 'windows-nt))
(defvar meadow-p (featurep 'meadow))
(defvar windows-p (or cygwin-p nt-p meadow-p))
;; -----------------------------------------------------------
;;; load-path 追加用の変数/関数
;; @see http://sakito.jp/emacs/emacs23.html
;;
;; Emacs 22 以下用に user-emacs-directory を定義する.
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))
;;; load-path 追加用の関数
;; 読み込みたくないファイルは, 先頭に "." や "_" をつけると良い
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
;;  M-x list-load-path-shadows
;; で確認する.
(add-to-load-path
 ;; 分割した設定群の置き場所.
 "site-start.d"
 ;; 自作の小物など
 "local-lisp"
 ;; auto-install で install したファイル.
 "auto-install"
 ;; org-mode は常に最新版(Git HEAD)を load する.
 "site-lisp/org-mode/lisp"
 ;; apel は無いと色々絶望的? <- ddskk の apel 依存ってどうなったのかしら
 "site-lisp/apel"
 )
;; -----------------------------------------------------------
;;; org-babel
(require 'org-install)
(defvar org-startup-dir (concat user-emacs-directory "site-start.d/"))
(org-babel-load-file (expand-file-name "init-startup.org" org-startup-dir))
;; ------------------------------------
;;; calculate bootup time/ スピード狂に捧ぐ.
;;
;; 目標: 3000ms 圏内
;;
(when emacs23-p
  (defun message-startup-time ()
    (message
     "Emacs loaded in %dms"
     (/ (- (+ (third after-init-time) (* 1000000 (second after-init-time)))
           (+ (third before-init-time) (* 1000000 (second before-init-time))))
        1000)))
  (add-hook 'after-init-hook 'message-startup-time))
;;; init.el ends here

;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;
;; Copyright (C) 2010-2011  Youhei SASAKI
;; $Lastupdate: 2011/11/29 15:39:14$
;;
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; Keywords:
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; .emacs.d/init.el で (setq skk-user-directory "~/.emacs.d/etc/skk") してい
;; るので, 設定ファイル群は以下の通り.
;; skk-init-file          ~/.emacs.d/etc/skk/init
;; skk-jisyo              ~/.emacs.d/etc/skk/jisyo
;; skk-backup-jisyo       ~/.emacs.d/etc/skk/jisyo.bak
;; skk-emacs-id-file      ~/.emacs.d/etc/skk/emacs-id
;; skk-record-file        ~/.emacs.d/etc/skk/record
;; skk-study-file         ~/.emacs.d/etc/skk/study
;; skk-study-backup-file  ~/.emacs.d/etc/skk/study.bak
;;
;;; Code:
(eval-when-compile
 (require 'skk-vars)
 (require 'skk-autoloads)
  )
(setq default-input-method "japanese-skk")
;; sticky shift!
(setq skk-sticky-key ";")
;; 日本語表示しない
(setq skk-japanese-message-and-error nil)
;; メニューを日本語にしない -> toolbar 非表示だし.
;; (setq skk-show-japanese-menu t)
;; 注釈の表示
(setq skk-show-annotation nil)
;; ddskk 起動時のみ, インクリメンタルサーチを使用
;;; Isearch setting.
(add-hook 'isearch-mode-hook
          #'(lambda ()
              (when (and (boundp 'skk-mode)
                         skk-mode
                         skk-isearch-mode-enable)
                (skk-isearch-mode-setup))))
(add-hook 'isearch-mode-end-hook
          #'(lambda ()
              (when (and (featurep 'skk-isearch)
                         skk-isearch-mode-enable)
                (skk-isearch-mode-cleanup))))
;; migemo を使うので skk-isearch にはおとなしくしていて欲しい
(setq skk-isearch-start-mode 'latin)
(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
(add-hook 'isearch-mode-hook 'skk-isearch-mode-cleanup)
;; インジケータを左端に.
(setq skk-status-indicator 'left)
;; 半角カナを入力
(setq skk-use-jisx0201-input-method t)
;; skk モードの表示のカスタマイズ
(setq skk-latin-mode-string "[aa]")
(setq skk-hiragana-mode-string "[jj]")
(setq skk-katakana-mode-string "[qq]")
(setq skk-jisx0208-latin-mode-string "[AA]")
(setq skk-jisx0201-mode-string "[QQ]")
(setq skk-indicator-use-cursor-color nil)
(setq skk-show-inline 'vertical)
(when skk-show-inline
  (if (boundp 'skk-inline-show-face)
      (setq
       skk-inline-show-background-color "black")))
;; Enter で改行しない
(setq skk-egg-like-newline t)
;;"「"を入力したら"」"も自動で挿入
(setq skk-auto-insert-paren t)
;; カーソルには色をつけない
(setq skk-use-color-cursor nil)
;; 句読点変換ルール
(setq skk-kuten-touten-alist
  '(
    (jp . ("." . "," ))
    (en . ("." . ","))
    ))
;; jp でも ".""," を使う.
(setq skk-kutouten-type 'en)
;; 全角記号の変換
(setq skk-rom-kana-rule-list
      (append skk-rom-kana-rule-list
              '(("!" nil "!")
                (":" nil ":")
                (";" nil ";")
                ("?" nil "?")
                ("z " nil "　")
                ("\\" nil "\\")
                )))
;; 全角英語モードで U+FF0D, U+FF5E を入力する?
;; (when (not (string< mule-version "6.0"))
;;   (aset skk-jisx0208-latin-vector ?- (string #xFF0D))
;;   (aset skk-jisx0208-latin-vector ?~ (string #xFF5E)))
;; @で挿入する日付表示を西暦&半角に
(setq skk-date-ad t)
(setq skk-number-style nil)
;; 送り仮名が厳密に正しい候補を優先
(setq skk-henkan-strict-okuri-precedence t)
;; 辞書の共有
(setq skk-share-private-jisyo t)
;; 辞書サーバの指定
(when (string-match system-name "daphne")
  (setq skk-server-host "127.0.0.1")
  (setq skk-server-portnum "1178")
  (setq skk-large-jisyo nil)
  ;; server completion
  (add-to-list 'skk-search-prog-list
               '(skk-server-completion-search) t)
  (add-to-list 'skk-search-prog-list
               '(skk-comp-by-server-completion) t)
  )
(when (not (string-match system-name "daphne"))
  (setq skk-large-jisyo "~/.skkdic/SKK-JISYO.L")
  ;; ;; Debian 辞書の追加
  ;; (add-to-list
  ;;  'skk-search-prog-list
  ;;  '(skk-search-jisyo-file "~/.skkdic/SKK-JISYO.debian" 10000) t)
  ;; 地球物理辞書の追加
  (add-to-list
   'skk-search-prog-list
   '(skk-search-jisyo-file "~/.skkdic/SKK-JISYO.chibutsu" 10000) t)
   ;; 天文・物理辞書(tandic4)の追加
  (add-to-list
   'skk-search-prog-list
   '(skk-search-jisyo-file "~/.skkdic/SKK-JISYO.tanudic4" 10000) t)
  ;; はてな辞書
  (add-to-list
   'skk-search-prog-list
   '(skk-search-jisyo-file "~/.skkdic/SKK-JISYO.hatenakey" 10000) t)
  ;; 2ch 顔文字辞書
  (add-to-list
   'skk-search-prog-list
   '(skk-search-jisyo-file "~/.skkdic/SKK-JISYO.matsucon" 10000) t)
  )
;; 辞書登録の際に送り仮名を削除
(setq skk-check-okurigana-on-touroku 'auto)
;;漢字登録のミスをチェックする
(setq skk-check-okurigana-on-touroku t)
;;; キーバインド
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-mode)
(global-set-key "\C-j" 'skk-mode)
(global-set-key "\C-\\" 'skk-mode)

;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;; color-theme-uwabami.el

;; Copyright(C) 2011 Youhei SASAKI All rights reserved.
;; $Id: $

;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; Keywords:
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
(eval-when-compile
  (require 'color-theme))
(defun color-theme-uwabami ()
  "My color theme  - @see https://github.com/uwabami/dot-emacs.d/blob/master/local-lisp/color-theme-uwabami.el"
  (interactive)
  (color-theme-install
   '(color-theme-uwabami
     (
      ;; ターミナルでの使用を考えて, 背景は設定しない.
      ;; 背景色は frame の設定で行なう
      ;; (background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (foreground-color . "#f6f3e8")
      (forground-color . "#CCCCCC")
      (cursor-color . "#FF0000")
      (mouse-color . "black"))
     (default ((t (nil))))
     (blank-space-face ((t (:background "LightGray"))))
     (blank-tab-face ((t (:background "green" :foreground "black"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (border-glyph ((t (nil))))
     ;; リージョン
     (region ((t (:background "#435869"))))
     ;; モードライン
     (modeline ((t (:background "#333333" :foreground "#CCCCCC" :italic nil :bold nil))))
     ;; モードライン(非アクティブ)
     (modeline-inactive ((t (:background "#333333" :forground "#555555" :italic nil :bold nil))))
     (modeline-mousable ((t (:background "light goldenrod" :foreground "dim gray" :italic nil :bold nil))))
     (modeline-mousable-minor-mode ((t (:background "dim gray" :foreground "light goldenrod" :italic nil :bold nil))))
     ;; コメント
     (font-lock-comment-face ((t (:foreground "#888888" :italic nil :bold nil))))
     (font-lock-comment-delimiter-face ((t (:foreground "#888888" :italic nil :bold nil))))
     ;; 文字列
     (font-lock-string-face ((t (:foreground "#7FFF7F" :italic nil :bold nil))))
     ;; 関数
     (font-lock-function-name-face ((t (:foreground "#BF7FFF" :italic nil :bold nil))))
     ;; キーワード
     (font-lock-keyword-face ((t (:foreground "#FF7F7F" :italic nil :bold nil))))
     ;; 定数
     (font-lock-constant-face ((t (:foreground "#FFBF7F" :italic nil :bold nil))))
     ;; 変数
     (font-lock-variable-name-face ((t (:foreground "#7F77FF" :italic nil :bold nil))))
     ;; 型
     (font-lock-type-face ((t (:foreground "#FFFF7F" :italic nil :bold nil))))
     ;; プリプロセッサ
     (font-lock-preprocessor-face ((t (:foreground "#E5786D" :italic nil :bold nil))))
     ;; 警告
     (font-lock-warning-face ((t (:foreground "#FF00FF" :italic nil :bold nil))))
     ;; 組み込み関数
     (font-lock-builtin-face ((t (:foreground "#C080D0" :italic nil :bold nil))))
     ;; char
     (font-lock-negation-char-face ((t (:foreground "#E7F6DA" :italic nil :bold nil))))
     ;; doc-string
     (font-lock-negation-doc-face ((t (:foreground "#99968B" :italic nil :bold nil))))
     ;; 参照
     (font-lock-reference-face ((t (:foreground "#7B68EE" :italic nil :bold nil))))
     ;; リンク
     (font-lock-link-face ((t (:foreground "#8AC6F2" :italic nil :bold nil))))
     ;; 正規表現
     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062" :italic nil :bold nil))))
     (font-lock-regexp-grouping-construct ((t (:foreground "#FF0000" :italic nil :bold nil))))
     ;; 対応する括弧の色
     (show-paren-match ((t (:foreground "#DAA520" :italic nil :bold t))))
     ;; 以下, コレなんだっけ?
     (highline-face ((t (:background "#777777"))))
     (font-lock-lazy-highlight ((t (:forground "#000000" :background "#FFFF00"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (toolbar ((t (nil))))
     )))
(provide 'color-theme-uwabami)
;;; template.el ends here

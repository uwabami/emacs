;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-mew.el
;;
(defconst elscreen-mew-version "0.1.2 (Jun 02, 2008)")
;;
;; Author:   Takashi Masuda <masutaka@nifty.com>
;; Created:  May 20, 2008
;; Revised:  Jun 02, 2008

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'elscreen-mew)
(require 'elscreen)


;;; User Customizable Variables:

(defcustom elscreen-mew-mode-to-nickname-alist
  '(("^mew-draft-mode$" .
     (lambda ()
       (format "Mew(%s)" (buffer-name (current-buffer)))))
    ("^mew-" . "Mew"))
  "*Alist composed of the pair of mode-name and corresponding screen-name."
  :type '(alist :key-type string :value-type (choice string function))
  :tag "Mew major-mode to screen nickname alist"
  :set (lambda (symbol value)
         (custom-set-default symbol value)
         (elscreen-rebuild-mode-to-nickname-alist))
  :group 'mew)
(elscreen-set-mode-to-nickname-alist 'elscreen-mew-mode-to-nickname-alist)


;;; Code:

(defadvice mew-buffer-message (after
			       mew-buffer-message-after-advice
			       activate)
  (setq ad-return-value
	(format "%s[%d]" ad-return-value (elscreen-get-current-screen))))

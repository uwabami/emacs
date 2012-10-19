;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;;
;; @see https://gist.github.com/762297
;;
(require 'cl)
(eval-and-compile (require 'popup))
(defvar popup-color-string
  (let ((x 9) (y 3))
    (mapconcat 'identity
               (loop with str = (make-string x ?\ ) repeat y collect str)
               "\n"))
  "*String displayed in tooltip.")

(defun popup-color-at-point ()
  "Popup color specified by word at point."
  (interactive)
  (let ((word (word-at-point))
        (bg (plist-get (face-attr-construct 'popup-tip-face) :background)))
    (when word
      (unless (member (downcase word) (mapcar #'downcase (defined-colors)))
        (setq word (concat "#" word)))
      (set-face-background 'popup-tip-face word)
      (message "%s: %s"
               (propertize "Popup color"
                           'face `(:background ,word))
               (propertize (substring-no-properties word)
                           'face `(:foreground ,word)))
      (popup-tip popup-color-string)
      (set-face-background 'popup-tip-face bg))))

(global-set-key (kbd "C-c c") 'popup-color-at-point)

(provide 'popup-color-at-point)

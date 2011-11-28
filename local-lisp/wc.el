;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-
;; word counter
;; $Id: wc.el,v 1.2 2003/09/19 14:15:53 yamauchi Exp $
;; Copyright (C) 2000-2003 Yamauchi Hitoshi, 山内 斉
;;               2011 Youhei SASAKI, add (provide 'wc)
;; If you see the Japanese commented part, you can exchange the
;; commented line instead of English line.
;;
;; License: I(Youhei) don't know. May be some kind of FLOSS License
;; Please contact
;;
(defun count-chars-region (beg end)
  "word counter:wc:リージョン内の単語数、文字数、バイト数、行数を表示する"
  (interactive "r")
  (let ((k 0)(w 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (setq k (+ k 1))
        (forward-char 1))
      (goto-char beg)
      (while (< (point) end)
        (setq w (+ w 1))
        (forward-word 1))
      (message (format
                "%d 語, %d 文字, %d バイト, %d 行"
                w k (- end beg)(count-lines beg end))))))
(provide 'count-char-region)
;;; wc.el ends here

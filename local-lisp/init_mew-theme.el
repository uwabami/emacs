;;;; COPY FROM HERE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight header
;;;

(mew-setface-bold header-subject
  :tty "#e5786d" :light "Firebrick" :dark "#e5786d")

(mew-setface-bold header-from
  :tty "#cae682" :light "DarkOrange4" :dark "#cae682")

(mew-setface-bold header-date
  :tty "#95e454" :light "Forest#95e454" :dark "#95e454")

(mew-setface-bold header-to
;  :tty "magenta" :light "DarkViolet" :dark "violet")
  :tty "violet" :light "DarkViolet" :dark "violet")

(mew-setface-bold header-key
;  :tty "#95e454" :light "ForestGreen" :dark "#95e454")
  :tty "#95e454" :light "ForestGreen" :dark "#95e454")

(mew-setface-bold header-private)

(mew-setface-bold header-important
;  :tty "cyan" :light "MediumBlue" :dark "SkyBlue")
  :tty "cyan" :light "MediumBlue" :dark "cyan")

(mew-setface-bold header-marginal
  :tty "gray70" :light "gray70" :dark "gray70")

(mew-setface-bold header-warning
  :tty "#e5786d" :light "#e5786d" :dark "#e5786d")

(mew-setface-bold header-xmew
;  :tty "#cae682" :light "chocolate" :dark"chocolate")
  :tty "#cae682" :light "chocolate" :dark "#cae682")

(mew-setface-bold header-xmew-bad
  :tty "#e5786d" :light "#e5786d" :dark "#e5786d")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight body

(mew-setface body-url
  :tty "#e5786d" :light "Firebrick" :dark "#e5786d")

(mew-setface body-comment
  :tty "#8ac6f2" :light "gray50" :dark "gray70")

(mew-setface body-cite1
  :tty "#95e454" :light "ForestGreen" :dark "#95e454")

(mew-setface body-cite2
  :tty "cyan" :light "MediumBlue" :dark "SkyBlue")

(mew-setface body-cite3
  :tty "magenta" :light "DarkViolet" :dark "violet")

(mew-setface body-cite4
  :tty "#cae682" :light "DarkOrange4" :dark "#cae682")

(mew-setface body-cite5
  :tty "#e5786d" :light "Firebrick" :dark "Orange#E5786d")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight mark
;;;

(mew-setface mark-review
  :tty "cyan" :light "MediumBlue" :dark "SkyBlue")

(mew-setface mark-escape
  :tty "magenta" :light "DarkViolet" :dark "violet")

(mew-setface mark-delete
  :tty "#e5786d" :light "Firebrick" :dark "Magenta")

(mew-setface mark-unlink
  :tty "#cae682" :light "DarkOrange4" :dark "#cae682")

(mew-setface mark-refile
  :tty "#95e454" :light "ForestGreen" :dark "#95e454")

(mew-setface mark-unread)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight eof
;;;

(mew-setface-bold eof-message
  :tty "#95e454" :light "ForestGreen" :dark "#95e454")

(mew-setface-bold eof-part
  :tty "#cae682" :light "DarkOrange4" :dark "#cae682")

(provide 'init_mew-theme)
;;;; COPY TO HERE

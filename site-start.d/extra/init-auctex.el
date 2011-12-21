
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.
(setq TeX-master t)

(setq TeX-default-mode 'japanese-latex-mode)
(setq japanese-TeX-command-default "pTeX")
(setq japanese-LaTeX-command-default "pLaTeX")
(setq japanese-LaTeX-default-style "jsarticle")
(setq TeX-engine-alist '((ptex "pTeX" "eptex" "platex" "eptex")
                         (uptex "upTeX" "euptex" "uplatex" "euptex")))
(setq TeX-engine 'ptex)
(setq TeX-view-program-list
      '(("pxdvi" "pxdvi -nofork -watchfile 1 -editor \"emacsclient +%%l %%f\" %d -sourceposition %n:%b")
        ;; ("TeXworks" "texworks %o")
        ;; ("Evince" "evince %o")
        ;; ("fwdevince" "evince_forward_search %o %n %b")
        ("Okular" "okular --unique \"file:%o#src:%n %b\"")
        ;; ("zathura" "zathura %o")
        ;; ("mupdf" "mupdf %o")
        ;; ("xpdf" "xpdf %o")
        ;; ("acroread" "acroread %o")
        ;; ("pdfopen" "pdfopen -viewer ar9-tab %o")
        ))
(setq TeX-view-program-selection
      '((output-dvi "pxdvi")
        (output-pdf "Okular")
        ))
;; (setq TeX-japanese-process-input-coding-system 'utf-8
;;       TeX-japanese-process-output-coding-system 'utf-8)
;; (add-hook 'LaTeX-mode-hook
;;           (function (lambda () (outline-minor-mode t))))
;; (add-hook 'japanese-latex-mode-hook
;;           (function (lambda () (outline-minor-mode t))))
;; (add-hook 'TeX-mode-hook
;;           (function (lambda () (outline-minor-mode t))))
;; (add-hook 'LaTeX-mode-hook 'visual-inline-mode)

(setq preview-image-type 'dvipng)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (TeX-source-correlate-mode 1)
                      (setq TeX-source-correlate-start-server t)
                      (add-to-list 'TeX-command-list
                                   '("pLaTeX" "platex -src-specials %t"
                                     TeX-run-TeX nil (latex-mode) :help "Run e-pLaTeX"))
                      (add-to-list 'TeX-command-list
                                   '("pdfpLaTeX" "platex -synctex=1 %t && dvipdfmx %d"
                                     TeX-run-TeX nil (latex-mode) :help "Run e-pLaTeX and dvipdfmx"))
                      (add-to-list 'TeX-command-list
                                   '("pdfpLaTeX2" "platex -synctex=1 %t && dvips -Ppdf -t a4 -z -f %d | convbkmk -g > %f && ps2pdf %f"
                                     TeX-run-TeX nil (latex-mode) :help "Run e-pLaTeX, dvips, and ps2pdf"))
                      (add-to-list 'TeX-command-list
                                   '("pBibTeX" "pbibtex %s"
                                     TeX-run-BibTeX nil t :help "Run pBibTeX"))
                      (add-to-list 'TeX-command-list
                                   '("Mendex" "mendex %s"
                                     TeX-run-command nil t :help "Create index file with mendex"))
                      (add-to-list 'TeX-command-list
                                   '("dviView" "pxdvi -nofork -watchfile 1 -editor \"emacsclient +%%l %%f\" %d -sourceposition %n:%b"
                                     TeX-run-discard-or-function t t :help "Run DVI Viewer"))
                      (add-to-list 'TeX-command-list
                                   '("Okular" "okular --unique \"file:%s.pdf#src:%n %b\""
                                     TeX-run-discard-or-function t t :help "Forward search with Okular"))
                      )))

(setq font-latex-fontify-sectioning 1.0)

(setq font-latex-fontify-script nil)

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)
(add-hook 'LaTeX-mode-hook '(lambda ()
                              (add-to-list 'TeX-expand-list
                                           '("%u" Okular-make-url))))
(defun Okular-make-url () (concat
                           "file://"
                           (expand-file-name (funcall file (TeX-output-extension) t)
                                             (file-name-directory (TeX-master-file)))
                           "#src:"
                           (TeX-current-line)
                           (expand-file-name (TeX-master-directory))
                           "./"
                           (TeX-current-file-name-master-relative)))

(setq bibtex-user-optional-fields
      '(("annote" "Personal annotation (ignored)")
        ("yomi" "Yomigana")
        ("location" "where it is (ignored)")
        ("memo" "Memorundum (ignored)")
        ))

(autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
;; Make RefTeX faster
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-plug-into-AUCTeX t)

(require 'zotexo)
(add-hook 'LaTeX-mode-hook 'zotexo-minor-mode)
(setq zotexo--auto-update-is-on t)

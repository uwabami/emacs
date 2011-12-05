
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-default-mode 'japanese-latex-mode)
(setq japanese-TeX-command-default "pTeX")
(setq japanese-LaTeX-command-default "pLaTeX")
(setq japanese-LaTeX-default-style "jsarticle")
(setq TeX-japanese-process-input-coding-system 'utf-8
      TeX-japanese-process-output-coding-system 'utf-8)
(add-hook 'LaTeX-mode-hook
          (function (lambda () (outline-minor-mode t))))
(add-hook 'japanese-latex-mode-hook
          (function (lambda () (outline-minor-mode t))))
(add-hook 'TeX-mode-hook
          (function (lambda () (outline-minor-mode t))))

(setq font-latex-fontify-sectioning 1.0)

(setq font-latex-fontify-script nil)

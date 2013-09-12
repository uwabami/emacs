include $(HOME)/.emacs.d/Mkinclude
BATCH		:= $(EMACS) --batch -l $(HOME)/.emacs.d/init.el --no-site-file 
__ALL_SRC__ := index.org
ELFILES		:= $(wildcard *.el)
ELCFILES	:= $(wildcard *.elc)

all: 00index.elc
	@rm -f *.el

00index.elc: 00index.org
	@$(BATCH) --eval '(mapc (lambda (x) (my:org-babel-tangle-and-compile-file (symbol-name x))) (quote ($(CURDIR)/$<)))'

clean:
	@rm -f $(ELCFILES) $(ELFILES) *.html *.tex *.org_archive  index.elc

distclean: clean

include $(HOME)/.emacs.d/Mkinclude
BATCH		:= $(EMACS) --batch -l $(HOME)/.emacs.d/init.el --no-site-file 
__ALL_SRC__ := index.org
ELFILES		:= $(filter-out index.el,$(wildcard *.el))
ELCFILES	:= $(filter-out index.elc,$(wildcard *.elc))

all: index.elc
	@rm -f *.el

index.elc: index.org
	@$(BATCH) --eval '(mapc (lambda (x) (my:org-babel-tangle-and-compile-file (symbol-name x))) (quote ($(CURDIR)/$<)))'

clean:
	@rm -f $(ELCFILES) $(ELFILES) *.html *.tex *.org_archive  index.elc

distclean: clean

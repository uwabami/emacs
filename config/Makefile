include $(HOME)/.emacs.d/Mkinclude
EXCLUDE		:= sitemap.org export-config.org sitemap.org~
BATCH		:= $(EMACS) --batch -l $(HOME)/.emacs.d/init.el --no-site-file
__ALL_SRC__ := $(filter-out $(EXCLUDE), $(wildcard *.org))
ORGFILES	:= $(__ALL_SRC__:%v.org=)
ELFILES		:= $(ORGFILES:%.org=%.el)
ELCFILES	:= $(ORGFILES:%.org=%.elc)

all: $(ELCFILES)

%.elc: %.org
	@$(BATCH) --eval '(mapc (lambda (x) (my:org-babel-tangle-and-compile-file (symbol-name x))) (quote ($(CURDIR)/$<)))'

clean:
	rm -f $(ELCFILES) *.html *.tex *.org_archive

distclean: clean
	rm -f $(ELFILES)

include $(HOME)/.emacs.d/Mkinclude
EXCLUDE		:= sitemap.org export-config.org sitemap.org~ README.org
BATCH		:= $(EMACS) --batch -l $(HOME)/.emacs.d/init.el --no-site-file
#__ALL_SRC__ := $(filter-out $(EXCLUDE),$(wildcard *.org))
__ALL_SRC__ := index.org
ORGFILES	:= $(__ALL_SRC__:%v.org=)
ELFILES		:= $(filter-out index.el,$(wildcard *.el))
# ELFILES		:= $(ORGFILES:%.org=%.el)
ELCFILES		:= $(filter-out index.elc,$(wildcard *.elc))
# ELCFILES	:= $(ORGFILES:%.org=%.elc)

all: index.elc

index.elc: index.org
	rm -f index.el index.elc
	@$(BATCH) --eval '(mapc (lambda (x) (my:org-babel-tangle-and-compile-file (symbol-name x))) (quote ($(CURDIR)/$<)))'

clean:
	@rm -f $(ELCFILES) $(ELFILES) *.html *.tex *.org_archive  index.elc

distclean: clean

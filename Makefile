# -*- mode: makefile -*-
EMACS	?= emacs

all: bootstrap init.elc

bootstrap: .permission-stamp
.permission-stamp:
	chmod 700 tmp
	touch $@
%.elc: %.el
	$(EMACS) -l $< -batch -f batch-byte-compile $<
init.el: README.org
	$(EMACS) -Q --batch \
	  --eval "(progn \
	            (require 'ob) \
	            (require 'ob-tangle) \
	            (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\")))"
	$(EMACS) -Q -batch -l $@

clean:
	(cd config && rm -fr *.el *.elc)
	rm -fr init.elc init.el
distclean: clean
	rm -fr packages/* && touch packages/.gitkeep
	rm -fr .*-stamp .*-use auto-save-list
recompile: clean all

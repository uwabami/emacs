# -*- mode: makefile -*-
EMACS	?= emacs
EL		= init-ddskk.el
EL		+= $(shell dpkg -l wl-beta 2>&1 | grep -q ^ii && echo init-wl.el )
EL		+= $(shell dpkg -l wl 2>&1 | grep -q ^ii && echo init-wl.el )
ELC		= $(EL:%.el=%.elc)

all: init.elc $(ELC)
$(EL): bootstrap
bootstrap: tmp/bootstrap-stamp
tmp/bootstrap-stamp: init.el
	mkdir -p tmp
	chmod 700 tmp
	emacs -q -l init.el --batch --eval '(kill-emacs)'
	touch $@
init.el: README.org
	$(EMACS) -Q -q --batch --eval \
	   "(progn \
		  (require 'ob-tangle) \
		  (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\"))"
%.elc: %.el
	$(EMACS) -q -l init.el -batch -f batch-byte-compile $<
# 	@rm -f $<

clean:
	rm -fr auto-save-list *.el *.elc *~

distclean: clean
	rm -fr pkg
	rm -fr tmp

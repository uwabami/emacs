# -*- mode: makefile -*-
EMACS	?= emacs
PKG		 = org-plus-contrib
PKG		+= $(shell dpkg -l ddskk 2>&1 | grep -q ^ii || echo ddskk )
EL		= init-ddskk.el
EL		+= $(shell dpkg -l wl-beta 2>&1 | grep -q ^ii && echo init-wl.el )
ELC		= $(EL:%.el=%.elc)

all: bootstrap init.elc
elc: $(ELC)
	@rm -f init-*.el
bootstrap: tmp/bootstrap-stamp
tmp/bootstrap-stamp: init.el
	mkdir -p tmp
	chmod 700 tmp
	@for p in $(PKG) ; do \
		$(EMACS) -q --batch --eval \
		  "(defconst pkg-install '$$p)" -l emacs-batch-install.el ;\
	done
	rm -f emacs-batch-install.el
	touch $@
init.el: README.org
	$(EMACS) -q --batch --eval \
	   "(progn \
		  (require 'ob-tangle) \
		  (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\")))"
%.elc: %.el
	$(EMACS) -l init.el -batch -f batch-byte-compile $<
recompile:
	touch README.org
	$(MAKE)
clean:
	rm -fr auto-save-list *.el *.elc *~
distclean: clean
	rm -fr packages
	rm -fr quelpa
	rm -fr tmp

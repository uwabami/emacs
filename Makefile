# -*- mode: makefile -*-
EMACS	?= emacs
PKG		 = org-plus-contrib
# PKG		+= $(shell dpkg -l ddskk 2>&1 | grep -q ^ii || echo ddskk )
# EL		= init-ddskk.el
EL		=
EL		+= $(shell dpkg -l wl-beta 2>&1 | grep -q ^ii && echo init-wl.el )
EL		+= $(shell dpkg -l wl 2>&1 | grep -q ^ii && echo init-wl.el )
ELC		= $(EL:%.el=%.elc)

all: bootstrap init.elc
elc: $(ELC)
	@rm -f init-*.el
bootstrap: tmp/bootstrap-stamp
tmp/bootstrap-stamp: init.el
	@mkdir -p tmp
	@chmod 700 tmp
	@if [ ! -f $@ ] ; then \
	  for p in $(PKG) ; do \
		  $(EMACS) -q --batch --eval \
		    "(defconst pkg-install '$$p)" -l emacs-batch-install.el ;\
	  done ;\
	fi
	@rm -f emacs-batch-install.el
	@touch $@
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
	$(MAKE) elc
clean:
	rm -fr auto-save-list *.el *.elc *~
distclean: clean
	rm -fr quelpa
	rm -fr elpa
	rm -fr tmp

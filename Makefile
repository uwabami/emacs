# -*- mode: makefile -*-
CASK	?= share/cask/bin/cask
EMACS	?= emacs
SRC		+= $(shell if dpkg -l wl-beta 2>&1 | grep -q ^ii ; then echo init-wl.org ; fi )
SRC		+= $(shell if dpkg -l ddskk 2>&1 | grep -q ^ii ; then echo init-ddskk.org ; fi)
EL		?= $(SRC:%.org=%.el)
ELC		?= init.elc $(SRC:%.org=%.elc)

all: bootstrap $(ELC)
bootstrap: tmp/cask-stamp
tmp/cask-stamp:
	mkdir -p tmp
	$(EMACS) -Q -L share/cask -batch -f batch-byte-compile share/cask/cask.el
	$(CASK)
	touch $@
init.el: README.org
	$(EMACS) -q --batch --eval \
	   "(progn \
		  (require 'ob-tangle) \
		  (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\")))"
%.el: %.org
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
	rm -fr auto-save-alist *.el *.elc *~
distclean: clean
	rm -fr .cask
	rm -fr tmp

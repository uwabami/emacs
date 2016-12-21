# -*- mode: makefile -*-
CASK	?= share/cask/bin/cask
SRC		:= $(filter-out README.org, $(wildcard *.org))
EL		?= $(SRC:%.org=%.el)
ELC		?= $(SRC:%.org=%.elc)
EMACS	?= emacs

all: bootstrap init.el $(ELC) init.elc

bootstrap: tmp/cask-stamp
tmp/cask-stamp:
	mkdir -p tmp
	$(EMACS) -Q -L share/cask -batch -f batch-byte-compile share/cask/cask.el
	$(CASK)
	touch $@
init.el: README.org
	$(EMACS) -q --batch --eval \
	   "(progn \
	      (require 'ob) \
	      (require 'ob-tangle) \
	      (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\")))"
%.el: %.org
	$(EMACS) -q --batch --eval \
	   "(progn \
	      (require 'ob) \
	      (require 'ob-tangle) \
	      (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\")))"
%.elc: %.el
	$(EMACS) -l init.el -batch -f batch-byte-compile $<
clean:
	rm -fr auto-save-alist
	rm -fr *.el *.elc
distclean: clean
	rm -fr .cask
	rm -fr tmp

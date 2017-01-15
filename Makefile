# -*- mode: makefile -*-
EMACS	?= emacs
SRC		+= $(shell if dpkg -l wl-beta 2>&1 | grep -q ^ii ; then echo init-wl.org ; fi )
SRC		+= $(shell if dpkg -l ddskk 2>&1 | grep -q ^ii ; then echo init-ddskk.org ; fi)
EL		?= $(SRC:%.org=%.el)
ELC		?= $(SRC:%.org=%.elc)

all: bootstrap init.elc $(ELC)
bootstrap: tmp/bootstrap-stamp
tmp/bootstrap-stamp: init.el
	$(EMACS) -q --batch --eval \
		"(defconst pkg-install 'org-plus-contrib)" \
		-l emacs-batch-install.el
	rm -f emacs-batch-install.el
	touch $@
init.el: README.org
	$(EMACS) -q --batch --eval \
	   "(progn \
		  (require 'ob-tangle) \
		  (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\")))"
init.elc: init.el
	$(EMACS) -l init.el -batch -f batch-byte-compile $<
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
	rm -fr auto-save-list *.el *.elc *~
distclean: clean
	rm -fr packages
	rm -fr quelpa
	rm -fr tmp

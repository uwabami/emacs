# -*- mode: makefile -*-
EMACS	?= emacs

all: init.elc
init.elc: bootstrap
bootstrap: tmp/bootstrap-stamp
tmp/bootstrap-stamp: init.el
	git submodule update --init
	(cd modules/leaf && make check)
	(cd modules/feather && make check)
	mkdir -p tmp
	chmod 700 tmp
	touch $@
#	$(EMACS) -q --batch -l org-install.el
#	rm -f org-install.el
init.el: README.org
	$(EMACS) -Q -q --batch --eval \
	   "(progn \
		  (require 'ob-tangle) \
		  (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\"))"
%.elc: %.el
	$(EMACS) -q -l init.el -batch -f batch-byte-compile $<
# 	@rm -f $<




# EL		= init-ddskk.el
# EL		+= $(shell dpkg -l wl-beta 2>&1 | grep -q ^ii && echo init-wl.el )
# EL		+= $(shell dpkg -l wl 2>&1 | grep -q ^ii && echo init-wl.el )
# ELC		= $(EL:%.el=%.elc)

# all: init.elc $(ELC)
# bootstrap: tmp/bootstrap-stamp
# tmp/bootstrap-stamp: init.el
# 	mkdir -p tmp
# 	chmod 700 tmp
# 	$(EMACS) -q --batch -l org-install.el
# 	rm -f org-install.el
# 	touch $@
# $(EL): init.el

clean:
	rm -fr auto-save-list *.el *.elc *~

distclean: clean
	(cd modules/leaf && make clean)
	(cd modules/feather && make clean)
	rm -fr pkg
	rm -fr tmp

# -*- mode: makefile -*-
EMACS	?= emacs

all: bootstrap init.elc

bootstrap: .bootstrap-stamp
.bootstrap-stamp: .modules-stamp
.modules-stamp: .permission-stamp
	git submodule init
	git submodule update
	@echo "setup org-mode"
	(cd modules/org-mode && \
	  make compile EMACS="$(EMACS)" && \
	  make autoloads EMACS="$(EMACS)" )
	@echo "setup ddskkdic"
	[ -d modules/skkdic ] || mkdir modules/skkdic
	if [ ! -f modules/skkdic/SKK-JISYO.L ] ; then \
	  if [ -f /usr/share/skk/SKK-JISYO.L ] ; then \
	    ln -s /usr/share/skk/SKK-JISYO.L modules/skkdic/SKK-JISYO.L ;\
	  else \
	    wget -q -O - http://openlab.jp/skk/dic/SKK-JISYO.L.gz \
	    | gzip -d > modules/skkdic/SKK-JISYO.L ;\
	  fi  \
	fi
	touch $@
.permission-stamp:
	chmod 700 tmp
	touch $@
%.elc: %.el
	$(EMACS) -l $< -batch -f batch-byte-compile $<
init.el: README.org
	emacs -Q --batch -l "ob-tangle" \
	  --eval "(org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\"))"
conf-clean:
	(cd config && rm -fr *.el *.elc)
clean: conf-clean
	rm -fr init.elc init.el
distclean: clean
	( cd modules/org-mode && $(MAKE) clean )
	rm -fr modules/skkdic
	rm -fr .*-stamp .*-use auto-save-list
recompile: clean all

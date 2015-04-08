# -*- mode: makefile -*-
EMACS	?= emacs

all: bootstrap init.elc

bootstrap: tmp/.permission-stamp
tmp/.permission-stamp: tmp/.skkdic-stamp
	chmod 700 tmp
	touch $@
tmp/.skkdic-stamp:
	@echo "setup ddskkdic"
	[ ! -f tmp/.skkserver-use ] || touch $@ && exit 0
	[ -d share/skkdic ] || mkdir share/skkdic
	if [ ! -f share/skkdic/SKK-JISYO.L ] ; then \
	  if [ -f /usr/share/skk/SKK-JISYO.L ] ; then \
	    ln -s /usr/share/skk/SKK-JISYO.L share/skkdic/SKK-JISYO.L ;\
	  else \
	    wget -q -O - http://openlab.jp/skk/dic/SKK-JISYO.L.gz \
	    | gzip -d > share/skkdic/SKK-JISYO.L ;\
	  fi  \
	fi
	touch $@
%.elc: %.el
	$(EMACS) -q -l $< -batch -f batch-byte-compile $<
init.el: README.org
	$(EMACS) -q --batch \
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
	rm -fr share/skkdic
	rm -fr tmp/.*-stamp tmp/.*-use auto-save-list
recompile: clean all

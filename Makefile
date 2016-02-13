# -*- mode: makefile -*-
EMACS	?= emacs
SRC	?= $(wildcard config/*.org)
EL		?= $(SRC:%.org=%.el)
ELC	?= $(SRC:%.org=%.elc)

all: bootstrap $(EL) init.elc

%.el: %.org
	$(EMACS) -Q --batch --eval \
	  "(progn \
	     (require 'ob) \
	     (require 'ob-tangle) \
	     (org-babel-tangle-file \"$<\" \"$(patsubst config/%,%,$@)\" \"emacs-lisp\")))"
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
init.elc: tmp/.el-get-cli-run-stamp
	$(EMACS) -q -l $< -batch -f batch-byte-compile init.el
el-get-cli-run: tmp/.el-get-cli-run-stamp
tmp/.el-get-cli-run-stamp: init.el
	EMACS=$(EMACS) share/el-get-cli/bin/elget -f $< install
	touch $@
init.el: README.org
	$(EMACS) -q --batch \
	  --eval "(progn \
	            (require 'ob) \
	            (require 'ob-tangle) \
	            (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\")))"
clean:
	rm -f $(EL) $(ELC)
	rm -fr init.elc init.el
distclean: clean
	rm -fr packages/* && touch packages/.gitkeep
	rm -fr share/skkdic
	rm -fr tmp/.*-stamp tmp/.*-use auto-save-list && touch tmp/.gitkeep
	mkdir -p tmp/skk && touch tmp/skk/.gitkeep
recompile:
	touch README.org
	$(MAKE) all

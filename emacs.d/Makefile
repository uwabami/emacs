# -*- mode: makefile -*-
__SRC__		:= $(wildcard *.el)
ELFiles		:= $(__SRC__:%v.el=)
ELCFiles	:= $(ELFiles:%.el=%.elc)
CURDIR		:= $(shell pwd)

EMACS	?= emacs

all: bootstrap $(ELCFiles)
	-@rm -f config/*.el ac-comphist.dat
	-@find . -name \-f | xargs rm -fr
bootstrap: .bootstrap-stamp
.bootstrap-stamp: .modules-stamp
	touch $@
.modules-stamp: .permission-stamp
	git submodule init
	git submodule update
	@echo "setup org-mode"
	(cd modules/org-mode && \
	  make compile EMACS="$(EMACS)" && \
	  make autoloads EMACS="$(EMACS)" )
	@echo "setup anything-config"
	(cd modules/anything-config && make )
	@echo "setup ddskkdic"
	[ -d modules/skkdic ] || mkdir modules/skkdic
	if [ ! -f modules/skkdic/SKK-JISYO.L ] ; then \
	  if [ -f /usr/share/skk/SKK-JISYO.L ] ; then \
	    ln -s /usr/share/skk/SKK-JISYO.L modules/skkdic/SKK-JISYO.L ;\
	  else \
	    wget -q -O - http://openlab.jp/skk/dic/SKK-JISYO.L.gz | gzip -d > modules/skkdic/SKK-JISYO.L ;\
	  fi  \
	fi
	touch $@
.permission-stamp:
	chmod 700 tmp
	touch $@

%.elc: %.el
	$(EMACS) -l $(HOME)/.emacs.d/init \
	  -batch -f batch-byte-compile $(CURDIR)/$<

recompile: clean all
conf-clean:
	rm -f config/*.elc config/*.el
clean: conf-clean
	rm -fr $(ELCFiles)
	-@find . -name \-f | xargs rm -fr
distclean: clean
	rm -fr auto-save-list modules/skkdic
	rm -fr packages/elpa packages/el-get packages/bundle
	rm -fr .*-stamp .*-use

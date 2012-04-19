# -*- mode: makefile -*-
__ALLSRC__		:= $(wildcard *.el)
include $(HOME)/.emacs.d/Mkinclude
INIT := $(HOME)/.emacs.d/init.el

all: $(ELCFiles)
	@[ -f ../elpa/package.elc ] || cp package.elc ../elpa/

update:
	git submodule foreach 'git checkout master ; git pull origin master; git pull --rebase'

%.elc: %.el
	@$(EMACS) -l $(INIT) -L ./ \
	  -q -no-site-file -batch -f batch-byte-compile $(CURDIR)/$<

clean: 
	rm -f $(ELCFiles) *~
	rm -f ../elpa/package.elc

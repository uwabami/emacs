# -*- mode: makefile -*-
__ALLSRC__	:= $(wildcard *.el)
include $(HOME)/.emacs.d/Mkinclude
TARGET_DIR	:= site-lisp config

all: TARGET $(ELCFiles)

update:
	git submodule foreach 'git fetch --all && git rebase origin/master'
	rm -f $(HOME)/.emacs.d/site-lisp/*-stamp

gc:
	git submodule foreach 'git gc ; git repack'

%.elc: %.el
	@$(EMACS) -l init.el -L $(TARGET_DIR) \
		-q -no-site-file -batch -f batch-byte-compile $(CURDIR)/$<

TARGET:
	@for d in $(TARGET_DIR) ;\
		do $(MAKE) -C $$d EMACS=$(EMACS) ;\
	done

clean:
	(cd config && $(MAKE) distclean)
	rm -f $(ELCFiles) *~

distclean: clean
	@for d in $(TARGET_DIR) ;\
		do $(MAKE) clean -C $$d ;\
	done
	rm -fr el-get/.loaddef.*
	rm -fr el-get/.status.*
	rm -fr el-get/*
	rm -fr tmp/*

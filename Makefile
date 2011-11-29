# -*- mode: makefile -*-
__ALLSRC__    := $(wildcard *.el)
include $(HOME)/.emacs.d/Mkinclude
TARGET_DIR		:= site-lisp auto-install site-start.d

all: TARGET $(ELCFiles)

update:
	git submodule foreach 'git checkout master ; git pull origin master; git pull --rebase'
	rm -f $(HOME)/.emacs.d/site-lisp/00build-stamp

%.elc: %.el
	@$(EMACS) -l init.el -L $(TARGET_DIR) \
	  -q -no-site-file -batch -f batch-byte-compile $(CURDIR)/$<

TARGET:
	@for d in $(TARGET_DIR) ;\
		do $(MAKE) -C $$d ;\
	done

clean:
	@for d in $(TARGET_DIR) ;\
		do $(MAKE) clean -C $$d ;\
	done
	rm -f $(ELCFiles) *~

distclean: clean

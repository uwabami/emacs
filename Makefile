# -*- mode: makefile -*-
include $(HOME)/.emacs.d/Mkinclude
__ALLSRC__    := $(wildcard *.el)
TARGET_DIR		:= site-lisp auto-install site-start.d

all: TARGET $(ELCFiles)

update:
	git submodule foreach 'git checkout master ; git pull origin master; git pull --rebase'

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

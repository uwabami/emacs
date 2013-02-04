# -*- mode: makefile -*-
__ALLSRC__	:= $(wildcard *.el)
include $(HOME)/.emacs.d/Mkinclude
TARGET_DIR	:= modules config

all: bootstrap TARGET $(ELCFiles)

check:
	@if [ $(shell cat $(HOME)/.emacs | wc -l) -ne 1 ]; then \
	  echo "Check Your Emacs !!! Local settings may be exists." ; exit 1 ; \
	fi

recompile: check
	( touch config/index.org && make)

bootstrap: .set_permission .bootstrap check
.bootstrap:
	( cd modules && $(MAKE) ) 
	$(EMACS) -nw
	touch $@

set_permission : .set_permission
.set_permission:
	chmod 700 tmp
	touch $@

update:
	git submodule foreach 'git fetch --all && git rebase origin/master'
	rm -f $(HOME)/.emacs.d/modules/*-stamp

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
	rm -rf $(ELCFiles) *~ auto-save-list

distclean: clean
	@for d in $(TARGET_DIR) ;\
		do $(MAKE) clean -C $$d ;\
	done
	rm -fr tmp/* el-get .bootstrap .set_permission

# -*- mode: makefile -*-
__ALLSRC__	:= $(wildcard *.el)
include $(HOME)/.emacs.d/Mkinclude
TARGET_DIR	:= modules config

all: bootstrap TARGET $(ELCFiles)

bootstrap: .bootstrap
.bootstrap:
	( cd modules && $(MAKE) ) 
	$(EMACS) -nw
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
	rm -rf $(ELCFiles) *~ bundle auto-save-list

distclean: clean
	@for d in $(TARGET_DIR) ;\
		do $(MAKE) clean -C $$d ;\
	done
	@rm -fr tmp/* bundle el-get 
	@rm -f .bootstrap

TARGET_DIR	:= $(subst ./, ,$(shell find . -maxdepth 1 -mindepth 1 -type d))
EXCLUDE		:= yasnippet org-jekyll offlineimap-el google-contacts-mew
TARGET_DIRS	:= $(filter-out $(EXCLUDE),$(TARGET_DIR))
include $(HOME)/.emacs.d/Mkinclude

all: TARGET

TARGET: 00build-stamp

00build-stamp: yasnippet
	@for d in $(TARGET_DIRS) ;\
		do $(MAKE) -C $$d EMACS=$(EMACS) ;\
	done
	touch $@

yasnippet:
	( cd yasnippet && rake compile ) 
clean:
	rm -f 00build-stamp

distclean: clean
	@for d in $(TARGET_DIRS) ;\
		do $(MAKE) clean -C $$d ;\
	done

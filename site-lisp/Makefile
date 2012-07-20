TARGET_DIR	:= $(subst ./, ,$(shell find . -maxdepth 1 -mindepth 1 -type d))
EXCLUDE		:= yasnippet org-jekyll offlineimap-el google-contacts-mew
TARGET_DIRS	:= $(filter-out $(EXCLUDE),$(TARGET_DIR))
include $(HOME)/.emacs.d/Mkinclude

all: 00build-stamp

00build-stamp: $(patsubst %,%-stamp,$(EXCLUDE))
	@for d in $(TARGET_DIRS) ;\
		do $(MAKE) -C $$d EMACS=$(EMACS) ;\
	done
	touch $@

yasnippet-stamp:
	@( cd yasnippet && rake compile )
	touch $@

org-jekyll-stamp:
	@( cd org-jekyll && \
	for f in `ls -1 *.el` ;\
	do $(EMACS) -L . -q -no-site-file -batch -f batch-byte-compile $$f ;\
	done )
	touch $@

offlineimap-el-stamp:
	@( cd offlineimap-el && \
	for f in `ls -1 *.el` ;\
	do $(EMACS) -L . -q -no-site-file -batch -f batch-byte-compile $$f ;\
	done )
	touch $@

google-contacts-mew-stamp:
	@( cd google-contacts-mew && \
	for f in `ls -1 *.el` ;\
	do $(EMACS) -L . -q -no-site-file -batch -f batch-byte-compile $$f ;\
	done )
	touch $@


clean:
	rm -f $(wildcard *-stamp)

distclean: clean
	@for d in $(TARGET_DIRS) ;\
		do $(MAKE) clean -C $$d ;\
	done
	@for d in $(EXCLUDE) ;\
	  do echo "cd $$d && rm -f *.elc" && (cd $$d && rm -f *.elc ) ;\
	done

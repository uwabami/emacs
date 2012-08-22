TARGET_DIR	:= $(subst ./, ,$(shell find . -maxdepth 1 -mindepth 1 -type d))
EXCLUDE		:= yasnippet offlineimap-el el-get org-mode
TARGET_DIRS	:= $(filter-out $(EXCLUDE),$(TARGET_DIR))
include $(HOME)/.emacs.d/Mkinclude

all: 00build-stamp

00build-stamp: $(patsubst %,%-stamp,$(EXCLUDE))
	@for d in $(TARGET_DIRS) ;\
	  do $(MAKE) -C $$d EMACS=$(EMACS) ;\
	  touch $$d-stamp ;\
	done
	touch $@

org-mode-stamp:
	@( cd org-mode && make )
	-@( cd org-mode/contrib/lisp && $(EMACS) -q -no-site-file -L . -batch -f batch-byte-compile *.el )
	touch $@

el-get-stamp:
	touch $@

yasnippet-stamp:
	touch $@

offlineimap-el-stamp:
	@( cd offlineimap-el && \
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

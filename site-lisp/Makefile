TARGET_DIR	:= $(subst ./, ,$(shell find . -maxdepth 1 -mindepth 1 -type d))
TARGET_DIRS	:= $(filter-out $(EXCLUDE),$(TARGET_DIR))
include $(HOME)/.emacs.d/Mkinclude

all: 00build-stamp

00build-stamp: $(patsubst %,%-stamp,$(TARGET_DIRS))
	touch $@

org-mode-stamp:
	@( cd org-mode && make compile && make autoloads )
	touch $@

clean:
	rm -f $(wildcard *-stamp)

distclean: clean
	@for d in $(TARGET_DIRS) ;\
		do $(MAKE) clean -C $$d ;\
	done

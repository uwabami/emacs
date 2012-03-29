TARGET_DIR	:= $(subst ./, ,$(shell find . -maxdepth 1 -mindepth 1 -type d))
EXCLUDE		:= yasnippet
TARGET_DIRS	:= $(filter-out $(EXCLUDE),$(TARGET_DIR))
include $(HOME)/.emacs.d/Mkinclude

all: TARGET

TARGET: 00build-stamp 

00build-stamp:
	@for d in $(TARGET_DIRS) ;\
		do $(MAKE) -C $$d ;\
	done
	touch $@
	
clean:
	rm -f 00build-stamp

distclean: clean
	@for d in $(TARGET_DIRS) ;\
		do $(MAKE) clean -C $$d ;\
	done

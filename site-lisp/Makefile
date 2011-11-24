TARGET_DIR	:= $(subst ./, ,$(shell find . -maxdepth 1 -mindepth 1 -type d))
__ALLSRC__	:= $(wildcard *.el)
include $(HOME)/.emacs.d/Mkinclude

all: $(ELCFiles) TARGET
%.elc: %.el
	@$(EMACS) \
	  -q -no-site-file -batch -f batch-byte-compile $(CURDIR)/$<
TARGET:
	@for d in $(TARGET_DIR) ;\
		do $(MAKE) -C $$d ;\
	done
clean:
	@for d in $(TARGET_DIR) ;\
		do $(MAKE) clean -C $$d ;\
	done
	rm -f $(ELCFiles)

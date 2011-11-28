__ALLSRC__ := $(wildcard *.el)
include $(HOME)/.emacs.d/Mkinclude

all: byte-compile

byte-compile: $(ELCFiles)
%.elc: %.el
	$(EMACS) \
	 -q -no-site-file -batch -f batch-byte-compile $(CURDIR)/$< 

clean:
	-rm -f *.elc

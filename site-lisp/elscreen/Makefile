__ALLSRC__ := $(wildcard *.el)
include $(HOME)/.emacs.d/Mkinclude

all: $(ELCFiles)
%.elc: %.el
	$(EMACS) -l PATH -L .\
		-q -no-site-file -batch -f batch-byte-compile $< 

clean:
	-rm -f *.elc

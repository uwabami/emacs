CURDIR	:= $(shell pwd)
__ALLSRC__ := $(wildcard *.el)
include $(HOME)/.emacs.d/Mkinclude

all: $(ELCFiles)

%.elc: %.el
	@$(EMACS) -L . -l $(HOME)/.emacs.d/init.el \
		-q -no-site-file -batch -f batch-byte-compile $(CURDIR)/$< 

clean:
	-rm -f *~ *.elc 

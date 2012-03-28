__ALLSRC__ := $(wildcard *.el)
include $(HOME)/.emacs.d/Mkinclude

all: byte-compile

byte-compile: $(ELCFiles)
%.elc: %.el
	EMACS=$(EMACS) /bin/sh ./elisp-comp $(__ALLSRC__)

clean:
	-rm -f *.elc

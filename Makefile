# -*- mode: makefile -*-
EMACS		?= emacs
EMACS_VER	?= $(shell emacs --version | head -1 | cut -f 3 --d \ )
EARLY_INIT	?= $(shell if [ `echo "$(EMACS_VER) >= 27" | bc` -eq 1 ] ; then echo yes; fi)
NATIVE_COMP	?= $(shell if [ `echo "$(EMACS_VER) >= 28.1" | bc` -eq 1 ] ; then echo yes; fi)
EL		=

ifeq ($(EARLY_INIT),yes)
EL		+= early-init.el
LOAD		?= -l early-init.el -l init.el
else
LOAD		?= -l init.el
endif

ifneq (,$(wildcard /etc/emacs/site-start.d/65wl-beta.el))
EL		+= init-wl.el
endif

ifeq ($(NATIVE_COMP),yes)
all: $(EL)
else
ELC		= $(EL:%.el=%.elc)
all: $(ELC) init.elc
endif

$(EL): init.el
init.el: README.org
	@mkdir -p ~/.cache/emacs
	@if [ ! -d ~/.cache/emacs/eln-cache ]; then \
		echo ";; mkdir ~/.cache/emacs/eln-cache"; mkdir ~/.cache/emacs/eln-cache ;\
	fi
	@if [ ! -L eln-cache ]; then \
		echo ";; ln -sf ~/.cache/emacs/eln-cache . "; ln -sf ~/.cache/emacs/eln-cache . ;\
	fi
	@if [ ! -d ~/.cache/emacs/tree-sitter ]; then \
		echo ";; mkdir ~/.cache/emacs/tree-sitter"; mkdir ~/.cache/emacs/tree-sitter ;\
	fi
	@if [ ! -L tree-sitter ]; then \
		echo ";; ln -sf ~/.cache/emacs/tree-sitter . "; ln -sf ~/.cache/emacs/tree-sitter . ;\
	fi
	@mkdir -p elpa
	$(EMACS) --batch --eval \
	   "(progn \
		  (require 'ob-tangle) \
		  (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\"))"
	$(EMACS) $(LOAD) --batch --eval '(kill-emacs)'

ifneq ($(NATIVE_COMP),yes)
%.elc: %.el
	$(EMACS) $(LOAD) -batch -f batch-byte-compile $<
endif

clean:
	rm -fr auto-save-list *.el *.elc *~ 

distclean: clean
	rm -fr elpa
	rm -fr eln-cache/*
	rm -f eln-cache
	rm -fr tree-sitter/*
	rm -f tree-sitter

skk-jisyo:
	mkdir -p ~/.cache/emacs/skk-jisyo
	wget "https://raw.githubusercontent.com/uasi/skk-emoji-jisyo/master/SKK-JISYO.emoji.utf8" \
		-O ~/.cache/emacs/skk-jisyo/SKK-JISYO.emoji.utf8
	wget "https://raw.githubusercontent.com/ymrl/SKK-JISYO.emoji-ja/master/SKK-JISYO.emoji-ja.utf8" \
		-O ~/.cache/emacs/skk-jisyo/SKK-JISYO.emoji-ja.utf8

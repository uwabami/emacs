# -*- mode: makefile -*-
# ----------------------------------------------------------------------------
EMACS		?= emacs
EMACS_VER 	?= $(shell $(EMACS) --version | head -1 | cut -d ' ' -f 3)
EARLY_INIT      ?= $(shell echo "$(EMACS_VER)" | awk '$$0 >= 27 {print "yes"}')
NATIVE_COMP     ?= $(shell echo "$(EMACS_VER)" | awk '$$0 >= 28.1 {print "yes"}')
TREESIT         ?= $(shell echo "$(EMACS_VER)" | awk '$$0 >= 29 {print "yes"}')
NATIVE_COMP_DIR ?= $(shell echo "$(EMACS_VER)" | awk '$$0 >= 30 {print "yes"}')
EL		=

# ----------------------------------------------------------------------------
# check: early-init
ifeq ($(EARLY_INIT),yes)
EL		+= early-init.el
LOAD		?= -l early-init.el -l init.el
else
LOAD		?= -l init.el
endif
# ----------------------------------------------------------------------------
# check: Wanderlust Debian package
ifneq (,$(wildcard /etc/emacs/site-start.d/65wl-beta.el))
EL		+= init-wl.el
endif
# ----------------------------------------------------------------------------
# check: native compile or byte compile
ifeq ($(NATIVE_COMP),yes)
all: $(EL)
else
ELC		= $(EL:%.el=%.elc)
all: $(ELC) init.elc
%.elc: %.el
	$(EMACS) $(LOAD) -batch -f batch-byte-compile $<
endif
# ----------------------------------------------------------------------------
# main part
$(EL): init.el
init.el: README.org
	@mkdir -p elpa
	$(EMACS) --batch --eval \
	   "(progn \
		  (require 'ob-tangle) \
		  (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\"))"
	@if [ x"$(TREESIT)" = x"yes" -a ! -f tree-sitter/libtree-sitter-elisp.so ] ; then \
		$(EMACS) --batch --eval \
		  "(progn \
			(setq treesit-language-source-alist \
			  '((elisp \"https://github.com/Wilfred/tree-sitter-elisp\"))) \
			(treesit-install-language-grammar 'elisp)))" ;\
	fi
	$(EMACS) $(LOAD) --batch --eval '(kill-emacs)'
# ----------------------------------------------------------------------------
# create subdirectory symlink
ifeq ($(NATIVE_COMP_DIR),yes)
README.org: tree-sitter
else
README.org: eln-cache tree-sitter
endif
tree-sitter:
	@if [ ! -d ~/.cache/emacs/tree-sitter ]; then \
		echo ";; mkdir ~/.cache/emacs/tree-sitter"; mkdir ~/.cache/emacs/tree-sitter ;\
	fi
	@if [ ! -L tree-sitter ]; then \
		echo ";; ln -sf ~/.cache/emacs/tree-sitter . "; ln -sf ~/.cache/emacs/tree-sitter . ;\
	fi
eln-cache:
	@mkdir -p ~/.cache/emacs
	@if [ ! -d ~/.cache/emacs/eln-cache ]; then \
		echo ";; mkdir ~/.cache/emacs/eln-cache"; mkdir ~/.cache/emacs/eln-cache ;\
	fi
	@if [ ! -L $@ ]; then \
		echo ";; ln -sf ~/.cache/emacs/eln-cache . "; ln -sf ~/.cache/emacs/eln-cache . ;\
	fi
# ----------------------------------------------------------------------------
clean:
	rm -fr auto-save-list *.el *.elc *~
distclean: clean
	rm -fr elpa
	rm -fr eln-cache/*
	rm -f eln-cache
	rm -fr tree-sitter/*
	rm -f tree-sitter
	rm -f ~/.cache/emacs/package-quickstart.*
# ----------------------------------------------------------------------------
skk-jisyo:
	mkdir -p ~/.cache/emacs/skk-jisyo
	wget "https://raw.githubusercontent.com/uasi/skk-emoji-jisyo/master/SKK-JISYO.emoji.utf8" \
		-O ~/.cache/emacs/skk-jisyo/SKK-JISYO.emoji.utf8
	wget "https://raw.githubusercontent.com/ymrl/SKK-JISYO.emoji-ja/master/SKK-JISYO.emoji-ja.utf8" \
		-O ~/.cache/emacs/skk-jisyo/SKK-JISYO.emoji-ja.utf8

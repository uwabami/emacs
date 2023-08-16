# -*- mode: makefile -*-
EMACS	?= emacs
EL		= early-init.el
EL		+= init-ddskk.el
ifneq (,$(wildcard /etc/emacs/site-start.d/65wl-beta.el))
EL		+= init-wl.el
endif
ELC		= $(EL:%.el=%.elc)

all: $(ELC) init.elc
# $(ELC): $(EL)
$(EL): init.el
init.el: README.org
	@mkdir -p ~/.cache/emacs
	@if [ ! -d ~/.cache/emacs/eln-cache ]; then \
		echo ";; mkdir ~/.cache/emacs/eln-cache"; mkdir ~/.cache/emacs/eln-cache ;\
	fi
	@if [ ! -L eln-cache ]; then \
		echo ";; ln -sf ~/.cache/emacs/eln-cache . "; ln -sf ~/.cache/emacs/eln-cache . ;\
	fi
	@mkdir -p elpa
	@mkdir -p el-get
	@mkdir -p share
	$(EMACS) --batch --eval \
	   "(progn \
		  (require 'ob-tangle) \
		  (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\"))"
	$(EMACS) -l init.el --batch --eval '(kill-emacs)'
%.elc: %.el
	$(EMACS) -l init.el -batch -f batch-byte-compile $<

clean:
	rm -fr auto-save-list *.el *.elc *~

distclean: clean
	rm -fr elpa
	rm -fr el-get
	rm -f eln-cache

skk-jisyo:
	mkdir -p ~/.cache/emacs/skk-jisyo
	wget "https://github.com/skk-dev/dict/raw/master/SKK-JISYO.L" \
		-O ~/.cache/emacs/skk-jisyo/SKK-JISYO.L

# -*- mode: makefile -*-
EMACS	?= emacs
EL		= init-ddskk.el
ifneq (,$(wildcard /etc/emacs/site-start.d/65wl-beta.el))
EL		+= init-wl.el
endif
EL		+= early-init.el
ELC		= $(EL:%.el=%.elc)

all: $(ELC) init.elc
$(EL): init.el
init.el: README.org
	@mkdir -p ~/.cache/emacs
	@mkdir -p pkg/elpa
	@mkdir -p pkg/el-get
	@mkdir -p share
	$(EMACS) -Q -q --batch --eval \
	   "(progn \
		  (require 'ob-tangle) \
		  (org-babel-tangle-file \"$<\" \"$@\" \"emacs-lisp\"))"
	$(EMACS) -q -l init.el --batch --eval '(kill-emacs)'
%.elc: %.el
	$(EMACS) -q -l init.el -batch -f batch-byte-compile $<
#	@rm -f $<

clean:
	rm -fr auto-save-list *.el *.elc *~

distclean: clean
	rm -fr pkg

skk-jisyo:
	mkdir -p ~/.cache/emacs/skk-jisyo
	wget "https://github.com/skk-dev/dict/raw/master/SKK-JISYO.L" \
		-O ~/.cache/emacs/skk-jisyo/SKK-JISYO.L

# -*- mode: makefile -*-
EMACS	?= emacs
EL		= init-ddskk.el
ifneq (,$(wildcard /etc/emacs/site-start.d/65wl-beta.el))
EL		+= init-wl.el
endif
ELC		= $(EL:%.el=%.elc)

all: $(ELC) init.elc
$(EL): init.el
init.el: README.org
	@mkdir -p tmp
	@chmod 700 tmp
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
	rm -fr tmp

skk-jisyo:
	mkdir -p tmp/skk-jisyo
	wget "https://github.com/skk-dev/dict/raw/master/SKK-JISYO.L" \
		-O tmp/skk-jisyo/SKK-JISYO.L

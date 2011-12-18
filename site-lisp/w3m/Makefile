# Generated automatically from Makefile.in by configure.
lispdir      = $(HOME)/.emacs.d/site-lisp/w3m
srcdir       = .
IGNORES = w3mhack.el
__ALLSRC__	= $(filter-out $(IGNORES), $(wildcard *.el))
include	$(HOME)/.emacs.d/Mkinclude
VANILLA_FLAG = -q -no-site-file --no-unibyte
FLAGS	=$(VANILLA_FLAG) -batch -l W3M-MK  -l $(srcdir)/w3mhack.el NONE
all: byte-compiled-stamp
byte-compiled-stamp: 
	[ ! -f $@ ] &&  $(MAKE) lisp
	touch $@
lisp: 
	$(MAKE) clean
	$(EMACS) $(FLAGS) -f w3mhack-compile

clean:
	rm -f *~ *.elc shimbun/*.elc w3m-load.el 
	rm -f byte-compiled-stamp

distclean: clean


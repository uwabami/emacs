# Generated automatically from Makefile.in by configure.
INSTALL      = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
lispdir      = $(HOME).emacs.d/site-lisp/w3m
srcdir       = .
SHELL        = /bin/sh

EMACS   = /usr/bin/emacs
VANILLA_FLAG = -q -no-site-file --no-unibyte
FLAGS   = $(VANILLA_FLAG) -batch -l $(srcdir)/w3mhack.el -l W3M-CFG

IGNORES = w3mhack.el


default: all
all: lisp
all-ja: lisp
lisp: Makefile
	env test ! -f w3m-util.elc -o w3m-util.elc -nt w3m-util.el || $(MAKE) clean
	env test ! -f w3m-proc.elc -o w3m-proc.elc -nt w3m-proc.el || $(MAKE) clean
	$(EMACS) $(FLAGS) -f w3mhack-compile

clean:
	rm -f *~ *.elc shimbun/*.elc w3m-load.el ;\

distclean: clean

## Rules for the developers to check the portability for each module.
.SUFFIXES: .elc .el

.el.elc:
	$(EMACS) $(FLAGS) -f batch-byte-compile $*.el


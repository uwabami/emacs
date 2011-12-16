#
# Makefile for FLIM.
#

PACKAGE = flim
API	= 1.14
RELEASE = 9

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l FLIM-MK

GOMI	= *.elc
FILES	= README.?? Makefile FLIM-MK FLIM-CFG FLIM-ELS *.el ChangeLog

VERSION	= $(API).$(RELEASE)

.PHONY: elc

all: elc

elc:
	$(EMACS) $(FLAGS) -f compile-flim

clean:
	-$(RM) $(GOMI)

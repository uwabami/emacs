#
# Makefile for SEMI kernel.
#
include $(HOME)/.emacs.d/Mkinclude
PACKAGE = semi
API	= 1.14
RELEASE = 6

FLIM_API= 1.14

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

FLAGS   = -batch -q -no-site-file -l SEMI-MK

GOMI	= *.elc

VERSION	= $(API).$(RELEASE)

elc:
	$(EMACS) $(FLAGS) -f compile-semi

clean:
	-$(RM) $(GOMI)

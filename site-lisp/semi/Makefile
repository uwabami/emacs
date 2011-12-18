#
# Makefile for SEMI kernel.
#
UNAME	:= $(shell uname)
ifeq ($(UNAME), Darwin)
	EMACS	:= /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
else
	EMACS	:= /usr/bin/emacs
endif
PACKAGE = semi
API	= 1.14
RELEASE = 6

FLIM_API= 1.14

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

FLAGS   = -batch -q -no-site-file -l SEMI-MK -L .

GOMI	= *.elc

VERSION	= $(API).$(RELEASE)

elc:
	$(EMACS) $(FLAGS) -f compile-semi

clean:
	-$(RM) $(GOMI)

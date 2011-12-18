#
# Makefile for APEL.
#

VERSION = 10.8
UNAME   := $(shell uname)
ifeq ($(UNAME), Darwin)
        EMACS   := /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
else
        EMACS   := /usr/bin/emacs
endif

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l APEL-MK

PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc

ARCHIVE_DIR_PREFIX = /home/kanji/tomo/public_html/lemi/dist

.PHONY:  all
all:
	@[ -f emu.elc ] || $(MAKE) elc

elc:
	$(EMACS) $(FLAGS) -f compile-apel \
		$(PREFIX) $(LISPDIR) $(VERSION_SPECIFIC_LISPDIR)

clean:
	-$(RM) $(GOMI)


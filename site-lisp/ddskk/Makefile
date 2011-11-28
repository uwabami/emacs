# Makefile: makefile for SKK.
#
# Maintainer: SKK Development Team <skk@ring.gr.jp>
# Version: $Id: Makefile,v 1.85 2011/07/01 11:32:21 skk-cvs Exp $
# $Lastupdate: 2011/11/28 11:23:36$
# Modify by Youhei SASAKI <uwabami@gfd-dennou.org> for personal use.

include $(HOME)/.emacs.d/Mkinclude
VERSION	:= 14.3
FLAGS	:= -batch -q -no-site-file -l SKK-MK
RM		:= /bin/rm -f

PHONY: all

all: skk-autoloads.elc

skk-autoloads.elc:
	@[ -f skk-autoloads.elc ] || $(MAKE) elc
elc:
	$(EMACS) $(FLAGS) -f SKK-MK-compile

clean:
	-$(RM) leim-list.el skk-autoloads.el skk-setup.el *.elc experimental/*.elc \
	auto-autoloads.el custom-load.el

# end of Makefile.

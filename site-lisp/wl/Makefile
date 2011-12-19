UNAME	:= $(shell uname)
ifeq ($(UNAME), Darwin)
	EMACS	:= /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
else
	EMACS	:= /usr/bin/emacs
endif
#
# Target directory to install the Wanderlust package.
# (Automatically detected if this line is unchanged.)
#
LISPDIR = $(HOME)/.emacs.d/site-lisp/wl/lisp
#LISPDIR = /usr/local/lib/mule/site-lisp

INFODIR = NONE
#INFODIR = /usr/local/share/info

# For XEmacs package.
PACKAGEDIR = NONE

# For XEmacs or Emacs 21: directory where icon files should go.
PIXMAPDIR = NONE


################# No need to modify following lines ####################
BATCHFLAG = -batch
FLAGS     = $(BATCHFLAG) -q -no-site-file

all: install-stamp

elc:
	$(EMACS) $(FLAGS) -l WL-MK -f compile-wl-package \
		$(LISPDIR) $(PIXMAPDIR)

install-stamp:
	[ ! -f $@ ] && $(MAKE) install
	touch $@

install: elc
	cp wl/*.elc lisp/
	cp elmo/*.elc lisp/
	cp utils/*.elc lisp/

clean:
	rm -f wl/*.elc wl/*~ wl/auto-autoloads.el wl/custom-load.el \
		wl/wl-news.el elmo/*.elc utils/*.elc \
		wl/*.elx elmo/*.elx utils/*.elx
	rm -f lisp/*.elc
	rm -f install-stamp

distclean: clean

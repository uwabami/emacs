#
# Please specify your Emacs here.
#
UNAME	:= $(shell uname)
ifeq ($(UNAME), Darwin)
	EMACS	:= /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
else
	EMACS	:= /usr/bin/emacs
endif
# To install Wanderlust for XEmacs 21 or later,
# running 'make install-package' is recommended.
# 'make install-package' refers $XEMACS instead of $EMACS.
XEMACS  = xemacs
#
# Target directory to install the Wanderlust package.
# (Automatically detected if this line is unchanged.)
#
LISPDIR = $(HOME)/.emacs.d/site-lisp/wl

INFODIR = NONE

# For XEmacs package.
PACKAGEDIR = NONE

# For XEmacs or Emacs 21: directory where icon files should go.
PIXMAPDIR = $(HOME)/.emacs.d/share/wl


################# No need to modify following lines ####################
BATCHFLAG = -batch
FLAGS     = $(BATCHFLAG) -q -no-site-file

all: byte-compiled-stamp
byte-compiled-stamp:
	[ ! -f $@ ] && $(MAKE) elc
	touch $@

elc:
	$(EMACS) $(FLAGS) -l WL-MK -f compile-wl-package \
		$(LISPDIR) $(PIXMAPDIR)

check:
	$(EMACS) $(BATCHFLAG) -l WL-MK -f check-wl \
		$(LISPDIR) $(PIXMAPDIR)

test:
	$(EMACS) $(FLAGS) -l WL-MK -f test-wl \
		$(LISPDIR) $(PIXMAPDIR)

compile-strict: clean-elc
	@args="$(FLAGS)";\
	args="$$args -L elmo -L wl";\
	echo "=============================================";\
	echo "Compiling the 1st stage-----without elc files";\
	echo "=============================================";\
	for i in `$(EMACS) $(FLAGS) -l WL-MK -f wl-examine-modules 2>/dev/null`;\
	  do\
	  j=`echo $$i| sed 's/elc$$/el/g'`;\
	  echo "$(EMACS) ARGS -f batch-byte-compile $$j";\
	  $(EMACS) $$args -f batch-byte-compile $$j;\
	  mv $$i $$j"x";\
	done;\
	for i in `echo elmo/*.elx wl/*.elx utils/*.elx`; do\
	  j=`echo $$i| sed 's/elx$$/elc/g'`;\
	  mv $$i $$j;\
	done;\
	echo "==============================================";\
	echo "Compiling the 2nd stage-----with all elc files";\
	echo "==============================================";\
	for i in `$(EMACS) $(FLAGS) -l WL-MK -f wl-examine-modules 2>/dev/null`;\
	  do\
	  j=`echo $$i| sed 's/elc$$/el/g'`;\
	  echo "$(EMACS) ARGS -f batch-byte-compile $$j";\
	  $(EMACS) $$args -f batch-byte-compile $$j;\
	done

clean-elc:
	rm -f wl/*.elc wl/*~ wl/auto-autoloads.el wl/custom-load.el \
		wl/wl-news.el elmo/*.elc utils/*.elc \
		wl/*.elx elmo/*.elx utils/*.elx

all: elc

clean: clean-elc
	rm -f byte-compiled-stamp

distclean: maintainer-clean

maintainer-clean: clean 

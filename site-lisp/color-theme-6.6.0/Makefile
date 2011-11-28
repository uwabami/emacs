# -*- Makefile -*-

# Borrowed from many emacs places

include Makefile.defs

SPECIAL = 
UNCOMPILED = 
AUTOLOADFILE = color-theme-autoloads
TESTING = 
THEMES_DIRECTORY = themes
THEMES_FILES := $(wildcard $(THEMES_DIRECTORY)/*.el)
ALLSOURCE := $(wildcard *.el) $(THEMES_FILES)
SOURCE	= $(filter-out $(SPECIAL) $(UNCOMPILED) $(TESTING),$(ALLSOURCE))
TARGET	= $(patsubst %.el,%.elc,$(SPECIAL) $(SOURCE))
MANUAL  = color-theme
MISC	= AUTHORS COPYING ChangeLog Makefile.defs Makefile $(AUTOLOADFILE).in
#AUTHORS CREDITS HISTORY NEWS README Makefile ChangeLog \
#ChangeLog.2005 ChangeLog.2004 ChangeLog.2003 ChangeLog.2002 \
#ChangeLog.2001 servers.pl color-theme-auto.in color-theme.texi

PHONY: all

all: lisp $(AUTOLOADFILE).elc

lisp: $(TARGET) 

autoloads: $(AUTOLOADFILE).elc

$(AUTOLOADFILE).el: $(AUTOLOADFILE).in #$(TARGET)
	cp $(AUTOLOADFILE).in $(AUTOLOADFILE).el
	rm -f $(AUTOLOADFILE).elc
	@$(EMACS) -q $(SITEFLAG) -batch \
		-l $(shell pwd | sed -e 's|^/cygdrive/\([a-z]\)|\1:|')/$(AUTOLOADFILE) \
		-f color-theme-generate-autoloads \
		$(shell pwd | sed -e 's|^/cygdrive/\([a-z]\)|\1:|')/$(AUTOLOADFILE).el . \
		$(THEMES_DIRECTORY)

$(AUTOLOADFILE).elc: $(AUTOLOADFILE).el
	@echo "Byte compiling the autoload file "$<
	@$(EMACS) -batch -q -f batch-byte-compile $^
	@echo "*******************************************************************"
	@echo "Autoloads up to date. Put the following lines in your configuration"
	@echo "file (~/.emacs for a single user) :"
	@echo
	@echo ${patsubst %, "(add-to-list 'load-path \""%"\")   ", $(LISPDIRS)}
	@echo "(require 'color-theme-autoload \""$(AUTOLOADFILE)"\")"
	@echo
	@rm -f $<~

%.elc: %.el
	@$(EMACS) $(OPTIONCOMPILE) \
	--eval '(setq load-path (cons "." load-path))' \
	-f batch-byte-compile $<

%.info: %.texi
	@echo "No doc yet !"
#	makeinfo $<

%.html: %.texi
	@echo "No doc yet !"
#	makeinfo --html --no-split $<

doc: $(MANUAL).info $(MANUAL).html
	@echo "No doc yet !"

clean:
	-rm -f themes/*.elc
	-rm -f *~ *.elc $(AUTOLOADFILE).el

realclean: clean
	-rm -f $(MANUAL).info $(MANUAL).html $(TARGET) $(SPECIAL)

distclean: realclean

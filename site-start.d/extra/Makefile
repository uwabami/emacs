include $(HOME)/.emacs.d/Mkinclude
EXCLUDE		:= 
BATCH		:= $(EMACS) --batch -l $(HOME)/.emacs.d/init.el --no-site-file
__ALL_SRC__ := $(filter-out $(EXCLUDE), $(wildcard *.org))
ORGFILES	:= $(__ALL_SRC__:%v.org=)
ELFILES		:= $(ORGFILES:%.org=%.el)
ELCFILES	:= $(ORGFILES:%.org=%.elc)

all: 
	@echo
	
clean: distclean
distclean: 
	rm -f $(ELFILES) $(ELCFILES)

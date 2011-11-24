include $(HOME)/.emacs.d/Mkinclude
BATCH		:= $(EMACS) --batch -l $(HOME)/.emacs.d/init.el --no-site-file -L
__ALL_SRC__	:= $(wildcard *.org)
ORG_FILES	:= $(__ALL_SRC__:%v.org=)
EL_FILES	:= $(ORG_FILES:%.org=%.el)

all: $(EL_FILES)

%.el: %.org
	@$(BATCH) \
	 --eval '(mapc (lambda (x) (org-babel-load-file (symbol-name x))) (quote ($<)))'

clean:
	rm -f $(EL_FILES)
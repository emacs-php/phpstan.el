EMACS ?= emacs
CASK ?= cask
ELS = phpstan.el flycheck-phpstan.el flymake-phpstan.el
AUTOLOADS = phpstan-autoloads.el
ELCS = $(ELS:.el=.elc)

%.elc: .cask %.el
	$(EMACS) -Q -batch -L . -f package-initialize -f batch-byte-compile $<

.cask: Cask
	CASK

all: clean autoloads $(ELCS)

autoloads: $(AUTOLOADS)

$(AUTOLOADS): $(ELCS)
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path) \
	   (package-generate-autoloads \"phpstan\" default-directory))"

clean:
	-rm -f $(ELCS) $(AUTOLOADS)

clobber: clean
	-rm -f .cask

.PHONY: all autoloads clean clobber

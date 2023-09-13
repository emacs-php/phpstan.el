EMACS ?= emacs
CASK ?= cask
EASK ?= eask

compile:
	$(EASK) compile

all: clean autoloads compile

autoloads:
	$(EASK) generate autoloads

clean:
	$(EASK) clean all

.PHONY: all autoloads clean

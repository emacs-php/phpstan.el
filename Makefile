EMACS ?= emacs
CASK ?= cask
EASK ?= eask

install:
	$(EASK) package
	$(EASK) install

compile:
	$(EASK) compile

all: clean autoloads install compile

autoloads:
	$(EASK) generate autoloads

clean:
	$(EASK) clean all

.PHONY: all autoloads clean

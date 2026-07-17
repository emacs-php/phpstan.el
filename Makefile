EMACS ?= emacs
CASK ?= cask
EASK ?= eask

install:
	$(EASK) package
	$(EASK) install
	$(EASK) install-deps

compile:
	$(EASK) compile

test:
	$(EASK) test ert ./test/*-test.el

all: clean autoloads install compile test

autoloads:
	$(EASK) generate autoloads

clean:
	$(EASK) clean all

.PHONY: all autoloads clean compile install test

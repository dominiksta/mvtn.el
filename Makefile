export EMACS=emacs

.PHONY: default compile clean test test-package run package run-package

# compile
# ----------------------------------------------------------------------

default: compile
compile:
	eldev compile

# clean
# ----------------------------------------------------------------------
clean:
	eldev clean
	rm -rf test/test-notes dist

# unit tests
# ----------------------------------------------------------------------
test:
	eldev test

test-package:
	eldev -p test

# load in default emacs
# ----------------------------------------------------------------------
run:
	eldev emacs

# packaging
# ----------------------------------------------------------------------
package:
	rm -f dist/*
	eldev package

run-package:
	eldev -p emacs

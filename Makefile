.SUFFIXES = .elc .el

EMACS = emacs
VERSION = 0.1
EL = mvtn.el mvtn-pkg.el
ELC = mvtn.elc mvtn-test.elc

# ----------------------------------------------------------------------
# clean
# ----------------------------------------------------------------------
clean:
	rm -rf *.elc *.tar mvtn-package/


# ----------------------------------------------------------------------
# dependencies
# ----------------------------------------------------------------------
mvtn-test.elc: mvtn.el mvtn-test.el

# ----------------------------------------------------------------------
# unit tests
# ----------------------------------------------------------------------
test: $(ELC)
	$(EMACS) -batch -Q -L . -l mvtn-test.elc -f ert-run-tests-batch


# ----------------------------------------------------------------------
# packaging
# ----------------------------------------------------------------------
mvtn-$(VERSION).tar: $(EL)
	rm -rf mvtn-$(VERSION)/
	mkdir mvtn-$(VERSION)/
	cp $(EL) mvtn-$(VERSION)/
	tar cf $@ mvtn-$(VERSION)
	rm -rf mvtn-$(VERSION)/

package: mvtn-$(VERSION).tar
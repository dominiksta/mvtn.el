.SUFFIXES = .elc .el

EMACS = emacs
VERSION = 0.1
EL = mvtn.el \
	mvtn-file-helpers.el \
	mvtn-ag.el \
	mvtn-rg.el \
	mvtn-pkg.el
TEST = test/mvtn-test.el \
	test/mvtn-test-helpers.el \
	test/mvtn-test-file-helpers.el

# ----------------------------------------------------------------------
# compile
# ----------------------------------------------------------------------
.el.elc:
	$(EMACS) -batch -Q -L . -L test -f batch-byte-compile $<

default: compile
compile: $(EL:.el=.elc) $(TEST:.el=.elc)

# ----------------------------------------------------------------------
# clean
# ----------------------------------------------------------------------
clean:
	rm -rf *.elc test/*.elc test/test-notes *.tar mvtn-package/


# ----------------------------------------------------------------------
# dependencies
# ----------------------------------------------------------------------
mvtn-test.elc: mvtn.el test/mvtn-test-helpers.el
mvtn-test-file-helpers.elc: mvtn.el test/mvtn-test-helpers.el test/mvtn-test.el
mvtn-file-helpers.elc: mvtn.el
mvtn-ag.elc: mvtn.el
mvtn-rg.elc: mvtn.el


# ----------------------------------------------------------------------
# unit tests
# ----------------------------------------------------------------------
test: compile
	cd test && \
	$(EMACS) -batch -Q -L .. -L . \
	-l mvtn-test.elc \
	-l mvtn-test-file-helpers.elc \
	-f ert-run-tests-batch


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
.SUFFIXES = .elc .el

EMACS   = emacs
VERSION = 0.1
EL      = mvtn.el \
          mvtn-compat.el \
          mvtn-file-helpers.el \
          mvtn-link-buttons.el \
          mvtn-tag-addons.el \
          mvtn-templates.el \
          mvtn-journal.el \
          mvtn-backlink-buffer.el \
          mvtn-ag.el \
          mvtn-rg.el
TEST    = test/mvtn-test.el \
          test/mvtn-test-helpers.el \
          test/mvtn-test-file-helpers.el \
          test/mvtn-test-templates.el \
          test/mvtn-test-journal.el \
          test/mvtn-test-tag-addons.el

# ----------------------------------------------------------------------
# compile
# ----------------------------------------------------------------------
.el.elc:
	$(EMACS) -batch -Q -L . -L test -f batch-byte-compile $<

default: compile
compile: $(EL:.el=.elc)

# ----------------------------------------------------------------------
# clean
# ----------------------------------------------------------------------
clean:
	rm -rf *.elc test/*.elc test/test-notes *.tar mvtn-package/


# ----------------------------------------------------------------------
# dependencies
# ----------------------------------------------------------------------
mvtn.elc: mvtn-compat.el
mvtn-test.elc: mvtn.el test/mvtn-test-helpers.el
mvtn-test-file-helpers.elc: mvtn.el test/mvtn-test-helpers.el test/mvtn-test.el
mvtn-test-tag-addons.elc: mvtn.el test/mvtn-test-helpers.el test/mvtn-test.el
mvtn-test-templates.elc: mvtn.el mvtn-templates.el test/mvtn-test.el
mvtn-file-helpers.elc: mvtn.el
mvtn-journal.elc: mvtn.el
mvtn-ag.elc: mvtn.el
mvtn-rg.elc: mvtn.el
mvtn-link-buttons.el: mvtn.el
mvtn-templates.el: mvtn.el mvtn-file-helpers.el
mvtn-backlink-buffer.el: mvtn.el mvtn-link-buttons.el


# ----------------------------------------------------------------------
# unit tests
# ----------------------------------------------------------------------
test: compile
	$(EMACS) -batch -Q -L . -L test \
	$(addprefix -l ,$(TEST)) \
	-f ert-run-tests-batch


# ----------------------------------------------------------------------
# load in default emacs
# ----------------------------------------------------------------------
run: compile
	cd test && \
	$(EMACS) -Q --debug-init -L .. -L . \
	$(addprefix -l ,$(TEST)) \
	--eval '(setq mvtn-note-directories mvtn-test-note-dirs)' \
	--eval '(mvtn-test-with-testfiles t)' \
	--eval '(find-file "mvtn-test.el")' \
	--eval '(split-window)' \
	--eval '(dired mvtn-test-note-dir)'

# ----------------------------------------------------------------------
# packaging
# ----------------------------------------------------------------------
mvtn-$(VERSION).tar: $(EL)
	rm -rf mvtn-$(VERSION)/
	mkdir mvtn-$(VERSION)/
	cp $(EL) mvtn-$(VERSION)/
	cp mvtn-pkg.el mvtn-$(VERSION)/
	cp -r templates mvtn-$(VERSION)/
	tar cf $@ mvtn-$(VERSION)
	rm -rf mvtn-$(VERSION)/

package: mvtn-$(VERSION).tar

run-package: package
	$(EMACS) -Q --debug-init \
	--eval '(package-install-file "mvtn-$(VERSION).tar")'

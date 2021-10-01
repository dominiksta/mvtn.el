;;; mvtn-eldev-emacs.el --- Code to only run for `eldev-emacs` -*- lexical-binding: t -*-

(require 'mvtn-test-helpers)
(require 'mvtn-test-setup)

(mvtn-test-with-testfiles t)
(require 'ivy)
(ivy-mode)

(provide 'mvtn-eldev-emacs)

;;; mvtn-eldev-emacs.el ends here
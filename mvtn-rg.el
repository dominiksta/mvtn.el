;;; mvtn-rg.el --- Support for rg (ripgrep) for mvtn -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to add support
;; for rg (ripgrep) (https://github.com/BurntSushi/ripgrep)
;; using rg.el (https://github.com/dajva/rg.el).

;;; Code:

(require 'mvtn)

(declare-function rg "ext:rg")
(defvar rg-executable)
(defvar rg-command-line-flags)

;;;###autoload
(defun mvtn-search-full-text-rg (string exclude-dirs)
  "Searches STRING using `rg' in `default-directory',
excluding directories specified as a list of strings in
DIRS. Fall back to `mvtn-search-full-text--grep' when
`rg-executable' is not found."
  (require 'rg)
  (if (not (executable-find rg-executable))
      (mvtn-search-full-text-grep string exclude-dirs)
    (let ((rg-command-line-flags
           (mapcar
            (lambda (dir) (format "-g=!%s" dir))
            exclude-dirs)))
    (rg string "everything" default-directory))))


(provide 'mvtn-rg)

;;; mvtn-rg.el ends here
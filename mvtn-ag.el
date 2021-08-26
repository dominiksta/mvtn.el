;;; mvtn-ag.el --- Support for ag (the silver searcher) for mvtn -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to add support
;; for ag (the silver searcher) (https://github.com/ggreer/the_silver_searcher)
;; using ag.el (https://github.com/Wilfred/ag.el).

;;; Code:

(require 'mvtn)

(declare-function ag/search "ext:ag")
(defvar ag-executable)
(defvar ag-arguments)

;;;###autoload
(defun mvtn-search-full-text-ag (string dirs)
  "Searches STRING using `ag-regexp' in `default-directory',
excluding directories specified as a list of strings in
DIRS. Fall back to `mvtn-search-full-text--grep' when
`ag-executable' is not found."
  (require 'ag)
  (if (not (executable-find ag-executable))
      (mvtn-search-full-text-grep string dirs)
    (ag/search string default-directory :regexp t :files dirs)))


(provide 'mvtn-ag)

;;; mvtn-ag.el ends here
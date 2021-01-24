;;; mvtn-ag.el --- Support for ag (the silver searcher) for mvtn -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to add support
;; for ag (the silver searcher) (https://github.com/ggreer/the_silver_searcher)
;; using ag.el (https://github.com/Wilfred/ag.el).

;;; Code:

(require 'mvtn)

(declare-function ag "ext:ag")
(defvar ag-executable)
(defvar ag-arguments)

;;;###autoload
(defun mvtn-search-full-text--ag (string exclude-dirs)
  "Searches STRING using `ag' in `default-directory', excluding
directories specified as a list of strings in DIRS. Fall back to
`mvtn-search-full-text--grep' when `ag-executable' is not found."
  (if (not (executable-find ag-executable))
      (mvtn-search-full-text--grep string exclude-dirs)
    (let ((ag-arguments (append '()
                                (mapcar
                                 (lambda (dir) (format "--ignore=%s" dir))
                                 exclude-dirs))))
      (print ag-arguments)
      (ag string default-directory))))


(provide 'mvtn-ag)

;;; mvtn-ag.el ends here
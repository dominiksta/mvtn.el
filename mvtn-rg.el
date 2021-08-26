;;; mvtn-rg.el --- Support for rg (ripgrep) for mvtn -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to add support
;; for rg (ripgrep) (https://github.com/BurntSushi/ripgrep)
;; using rg.el (https://github.com/dajva/rg.el).

;;; Code:

(require 'mvtn)
(require 'cl-macs)

(declare-function rg "ext:rg")
(defvar rg-executable)
(defvar rg-command-line-flags)
(defvar rg-build-command)


(defvar mvtn--rg-overwrite-command-command ""
  "Needed for `mvtn--rg-search-full-command'")
(defun mvtn--rg-overwrite-command (orig-fun &rest args)
  "Needed for `mvtn--rg-search-full-command'"
  (message "rg-build-command called with args %S" args)
  mvtn--rg-overwrite-command-command)

(defun mvtn--rg-search-full-command (dir command)
  "A dirty hack to allow specifying the complete command line for
`rg-run'. Necessary for `mvtn--rg-search-multi-directory'"
  (let ((default-directory dir))
    (advice-add 'rg-build-command :around 'mvtn--rg-overwrite-command)
    (setq mvtn--rg-overwrite-command-command command)
    (rg-run "tample sext" "everything" default-directory)
    (setq mvtn--rg-overwrite-command-command "")
    (advice-remove 'rg-build-command 'mvtn--rg-overwrite-command)))

(defun mvtn--rg-search-multi-directory (base-dir dirs search)
  "Search multiple directories using `rg'."
  ;; rg uses backslashes on windows
  (if (eq system-type 'windows-nt)
      (setq dirs (mapcar (lambda (el) (replace-regexp-in-string
                                  "/" "\\\\" el))
                         dirs)))
  ;; arguments are taken from `rg-build-command'
  (mvtn--rg-search-full-command
   base-dir (format "rg -S --color=always --colors=match:fg:red \
--colors=path:fg:magenta --colors=line:fg:green --colors=column:none -n \
--column --heading --no-config -e \"%s\" %s" search
(mapconcat 'shell-quote-argument dirs " "))))

;;;###autoload
(defun mvtn-search-full-text-rg (string dirs)
  "Searches STRING using `rg' in DIRS. Falls back to
`mvtn-search-full-text--grep' when `rg-executable' is not found."
  (require 'rg)
  (if (not (executable-find rg-executable))
      (mvtn-search-full-text-grep string dirs)
    (mvtn--rg-search-multi-directory default-directory dirs string)))


(provide 'mvtn-rg)

;;; mvtn-rg.el ends here
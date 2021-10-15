;;; mvtn-rg.el --- Support for rg (ripgrep) for mvtn -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to add support
;; for rg (ripgrep) (https://github.com/BurntSushi/ripgrep)
;; using rg.el (https://github.com/dajva/rg.el).

;;; Code:

(require 'mvtn)
(require 'cl-macs)

(declare-function rg-run "ext:rg")
(defvar rg-executable)
(defvar rg-command-line-flags)
(defvar rg-build-command)

;; ----------------------------------------------------------------------
;; interactive searches with rg.el
;; ----------------------------------------------------------------------

(defvar mvtn--rg-overwrite-command-command ""
  "Needed for `mvtn--rg-search-full-command'.")
(defun mvtn--rg-overwrite-command (orig-fun &rest args)
  "Needed for `mvtn--rg-search-full-command'.
ORIG-FUN will be `rg-run' and ARGS will be passed to it."
  (ignore orig-fun) ;; ignore argument -> no compiler warnings
  (message "rg-build-command called with args %S" args)
  mvtn--rg-overwrite-command-command)

(defun mvtn--rg-search-full-command (dir command)
  "A hack to allow specifying all cli options for `rg-run'.
Necessary for `mvtn--rg-search-multi-directory'.  DIR specifies
the directory to search in and COMMAND is the entire cli for rg."
  (let ((default-directory dir))
    (advice-add 'rg-build-command :around 'mvtn--rg-overwrite-command)
    (setq mvtn--rg-overwrite-command-command command)
    (rg-run "tample sext" "everything" default-directory)
    (setq mvtn--rg-overwrite-command-command "")
    (advice-remove 'rg-build-command 'mvtn--rg-overwrite-command)))

(defun mvtn--rg-search-multi-directory (base-dir dirs search)
  "Search multiple directories DIRS for SEARCH using `rg'.
`default-directory' will be set to BASE-DIR."
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
  "Search for STRING using `rg' in DIRS.
Falls back to `mvtn-search-full-text--grep' when `rg-executable'
is not found."
  (require 'rg)
  (if (not (executable-find rg-executable))
      (mvtn-search-full-text-grep string dirs)
    (mvtn--rg-search-multi-directory default-directory dirs string)))


;; ----------------------------------------------------------------------
;; programmatic searches with just the cli
;; ----------------------------------------------------------------------

(defcustom mvtn-search-full-text-rg-async-sort t
  "Wether to sort results in `mvtn-search-full-text-rg-async-command'.
Settings this to nil could improve performance because rg can
only run single threaded when the results are sorted.  However,
setting this to nil also means that results in for example the
backlink buffer will sometimes move around to different
positions, which can be disorienting."
  :type 'boolean :group 'mvtn)

;;;###autoload
(defun mvtn-search-full-text-rg-async-command (string)
  "Return an rg command for a full text search in all notes.
Searches for the regexp STRING."
  (let ((default-directory (plist-get (car mvtn-note-directories) :dir)))
    (append (list rg-executable "-nH0" "--no-heading"
                  "--before-context=2"
                  (format "--sort=%s" (if mvtn-search-full-text-rg-async-sort
                                          "path" "none"))
                  string)
            (mvtn--search-dirs))))


(provide 'mvtn-rg)

;;; mvtn-rg.el ends here
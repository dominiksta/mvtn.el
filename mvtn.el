;;; mvtn.el --- Minimum Viable Text Notes -*- lexical-binding: t -*-

;; Author: dominiksta, phga
;; Maintainer: dominiksta
;; Version: 0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/dominiksta/mvtn.el
;; Keywords: notes notetaking minimal links


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; A notetaking system with an emphasis on simplicity and flexibility in format.
;; Heavily inspired by Protesilaos Stavrou's `usls'
;; (https://protesilaos.com/codelog/2020-10-08-intro-usls-emacs-notes/).

;;; Code:
(eval-when-compile (require 'subr-x))
(require 'seq)
(require 'grep)

(declare-function mvtn-cv-prompt-for-tags "ext:mvtn-tag-addons")

(defcustom mvtn-note-directories
  '((:dir "~/mvtn/private" :name "prv" :structure
          ((:dir "fleeting" :datetree t)
           (:dir "zettelkasten" :datetree t)
           (:dir "devlog" :datetree t)
           (:dir "static" :datetree nil)))
    (:dir "~/mvtn/work" :name "wrk" :structure
          ((:dir "fleeting" :datetree t)
           (:dir "meetings" :datetree t)
           (:dir "devlog" :datetree t)
           (:dir "static" :datetree nil))))
  "The directory structure for all your mvtn notes.
Structurally, this is a list of plists, where each element contains the
following keys:

| Key        | Description                                                 |
|------------+-------------------------------------------------------------|
| :dir       | A directory on the file system.  This will not contain      |
|            | any notes itself; rather it is a base directory             |
|            | for the subdirectories specified in the :structure          |
|            | key.  It is intended to be used to separate between         |
|            | categories of notes that for some reason need to be in      |
|            | different places on the file system.  One might for example |
|            | want to store notes for their professional work on a        |
|            | separate, encrypted drive.  It is possible to specify drive |
|            | letters on windows (e.g. 'd:/mvtn/').                       |
| :name      | An abbreviated name of the directory.  This helps to        |
|            | improve the readabilty of a lot of listings and             |
|            | searches.  Can be any string of characters.                 |
| :structure | As already mentioned, this key describes subdirectories     |
|            | of :dir.                                                    |

Each element of list specified as the :structure key is itself
again a list of plists.  The required keys are listed in the
table below.

| Key       | Description                                                 |
|-----------+-------------------------------------------------------------|
| :dir      | A directory on the file system.  It is relative to the      |
|           | parent elements :dir.  Intended for further                 |
|           | categorisation beyond the parent elements :dir key.         |
|           | One might for example want to seperate a 'zettelkasten'     |
|           | aka 'slip-box' from a 'devlog' or meeting notes.            |
| :datetree | When this key is set to nil, notes will be created directly |
|           | in the directory :dir.  For long-term scalability, it is    |
|           | however strongly recommended to set this to t in most       |
|           | circumstances.  When set to t, notes will be created in     |
|           | subdirectories corresponding to the year the note is taken  |
|           | in.  When performing full-text searches or backlink         |
|           | searches, mvtn can then omit notes older then               |
|           | `mvtn-search-years', ensuring long-term scalability.        |
|           | Setting :datetree to t should only be used for notes that   |
|           | one wants to always be included in every search,            |
|           | regardless of their age.                                    |

Example:
\((:dir \"~/mvtn/private\" :name \"prv\" :structure
       ((:dir \"fleeting\" :datetree t)
        (:dir \"zettelkasten\" :datetree t)
        (:dir \"devlog\" :datetree t)
        (:dir \"static\" :datetree nil)))
\(:dir \"~/mvtn/work\" :name \"wrk\" :structure
      ((:dir \"fleeting\" :datetree t)
        (:dir \"meetings\" :datetree t)
        (:dir \"devlog\" :datetree t)
        (:dir \"static\" :datetree nil))))"
  :type 'string :group 'mvtn)

(defcustom mvtn-excluded-directories
  '(".git" ".svn" "ltximg")
  "A list of directories to ignore in note listings.
This affects `mvtn-open-note', `mvtn-insert-link' and everything
else using `mvtn-list-files-function'.  These directories might
contain linked pictures, LaTeX fragments or anything that is not
a note (NaN)."
  :type '(list :value-type string) :group 'mvtn)

(defcustom mvtn-default-file-extension "org"
  "The default extension of new mvtn notes.
Any extension is allowed, as mvtn is not dependant on `org-mode'
or markdown features, but it will be created and treated as a
plaintext file."
  :type 'string :group 'mvtn)

(defcustom mvtn-file-extension-templates
  '(("org" "#+TITLE: {title}
#+DATE: {date}
# mvtn_original_title :: {title}
# mvtn_original_id :: {timestamp}\n\n")
    ("md" "# {title}
Date: {date}
<!-- mvtn_original_title :: {title} -->
<!-- mvtn_original_id :: {timestamp} -->\n\n")
    ("" "title: {title}
date: {date}
mvtn_original_title :: {title}
mvtn_original_id :: {timestamp}
----------------------------------------------------------------------\n\n"))
  "A list of templates for new notes by extension.
When an extension is not matched, the fallback associated with
the empty string (\"\") is used.

Available substitutions:
{title}    : The notes title
{date}     : The notes date of creation
{timestamp}: The timestamp/id of the note

The first lines of these templates should not be changed, as they
are required for some functions such as
`mvtn-rename-current-file'."
  :type '(alist :value-type (group string)) :group 'mvtn)

(defcustom mvtn-list-files-order 'desc
  "The order of items in everything calling `mvtn-list-files'.
Either 'asc or 'desc.  Affects `mvtn-open-note',
`mvtn-insert-link' and everything else calling
`mvtn-list-files'."
  :type 'symbol :group 'mvtn)

(defcustom mvtn-search-years 3
  "The number of years many mvtn functions should consider by default.
Search in mvtn is by default limited to the previous n
years (including the current year).  This is done for long-term
scalability."
  :type 'number :group 'mvtn)

(defcustom mvtn-search-function 'mvtn-search-full-text-grep
  "The function used in for `mvtn-search-full-text'.
Its first argument should be the string to search for and the
second argument a list of directories (as strings) to exclude
from the search.  By default, `mvtn-search-full-text-grep' is
used.  Other options are `mvtn-search-full-text-ag' and
`mvtn-search-full-text-rg'."
  :type 'symbol :group 'mvtn)

(defcustom mvtn-list-files-function
  (if (and (not (eq system-type 'windows-nt))
           (executable-find find-program))
      'mvtn-list-files-function-find
    'mvtn-list-files-function-native)
  "The 'backend' function for `mvtn-list-files'.
This affects `mvtn-open-note', `mvtn-insert-link' and everything
else calling `mvtn-list-files'.

Takes one optional argument SEARCH which allows to only list
files matching this specific string

RETURN a list of all files (notes).

See `mvtn-list-files-function-native' and
`mvtn-list-files-function-find'."
  :type 'symbol :group 'mvtn)

(defcustom mvtn-link-actions '((" :: " mvtn-link-action-search))
  "A list of 'actions' that may be defined in a link.
Links may specify additional actions to be executed after
following the link.  These actions are defined in this alist.
The car of each element of this list is interpreted as a
seperator.  If this seperator (regexp) matches a link, then
everything following the seperator will be passed to its
associated function *after* following the link.  (Mvtn goes
through this list top to bottom and only executes the first
applicable action.)"
  :type '(alist :value-type (group symbol)) :group 'mvtn)

(defcustom mvtn-cv-enable nil
  "Wether to enable the use of a controlled vocabulary.
See `mvtn-cv-file' for further documentation."
  :type 'boolean :group 'mvtn)

(defvar mvtn--id-regexp "[[:digit:]]\\{8\\}-[[:digit:]]\\{6\\}"
  "A regexp matching a valid mvtn id.")
(defvar mvtn--link-regexp (concat "\\^\\^" mvtn--id-regexp "[^^]*\\^\\^")
  "A regexp matching valid mvtn links.")

(defun mvtn-note-dir-for-name (name)
  "RETURN the :dir property of a note directory.
The note directory must be in `mvtn-note-directories' and is
given by its NAME."
  (let ((candidates (seq-filter (lambda (el) (string-equal (plist-get el :name) name))
                                mvtn-note-directories)))
    (cond
     ((not candidates) (error "No note directory found for name: %s" name))
     ((> (length candidates) 1) (error "Conflicting note directory names: %s" name))
     (t (plist-get (car candidates) :dir)))))

(defun mvtn-short-note-dir-list (&optional datetreeonly)
  "Get all shortened note directory names in `mvtn-note-directories'.
When DATETREEONLY is non-nil, returns only directories with
:datetree set to t."
  (let ((result '()) (currstruct '()))
    (dolist (root-el mvtn-note-directories)
      (setq currstruct (plist-get root-el :structure))
      (when datetreeonly
        (setq currstruct (seq-filter (lambda (el) (plist-get el :datetree)) currstruct)))
      (setq result (append result (mapcar (lambda (el) (concat (plist-get root-el :name)
                                                          "/" (plist-get el :dir)))
                                          currstruct))))
    result))

(defun mvtn-expand-note-name (notename)
  "Translates a 'short' note path to a full file system path.
The 'short' path - given by NOTENAME - refers to a path shortened
by the :name property of the note directory in
`mvtn-note-directories'."
  (let ((split (split-string notename "/")))
    (concat (mvtn-note-dir-for-name (car split)) "/"
            (mapconcat 'identity (cdr split) "/"))))

(defun mvtn-current-timestamp (accuracy)
  "RETURN a timestamp for use in generating mvtn filenames.
ACCURACY is a symbol to define the format:
- ACCURACY = 'day   : {year}{month}{day}
- ACCURACY = 'hour  : {year}{month}{day}-{hour}
- ACCURACY = 'minute: {year}{month}{day}-{hour}{minute}
- ACCURACY = 'second: {year}{month}{day}-{hour}{minute}{second}"
  (declare (side-effect-free t))
  (cond ((eq accuracy 'day)
         (format-time-string "%Y%m%d"))
        ((eq accuracy 'hour)
         (format-time-string "%Y%m%d-%H"))
        ((eq accuracy 'minute)
         (format-time-string "%Y%m%d-%H%M"))
        ((eq accuracy 'second)
         (format-time-string "%Y%m%d-%H%M%S"))))


(defun mvtn-timestamp-field (timestamp field)
  "RETURN a FIELD in an mvtn TIMESTAMP.
FIELD may be one of 'year, 'month, 'day, 'hour, 'minute or
'second."
  (declare (side-effect-free t))
  (cond ((eq field 'year) (substring timestamp 0 4))
        ((eq field 'month) (substring timestamp 4 6))
        ((eq field 'day) (substring timestamp 6 8))
        ((and (eq field 'hour) (>= (length timestamp) 11)) (substring timestamp 9 11))
        ((and (eq field 'minute) (>= (length timestamp) 13)) (substring timestamp 11 13))
        ((and (eq field 'second) (>= (length timestamp) 15)) (substring timestamp 13 15))
        (t nil)))


(defun mvtn-template-for-extension (extension)
  "RETURN the template in `mvtn-file-extension-templates' for EXTENSION."
  (declare (side-effect-free t))
  (or (cadr (assoc extension mvtn-file-extension-templates))
      (cadr (assoc "" mvtn-file-extension-templates))))


(defun mvtn-substitute-template (template-string title date timestamp)
  "Substitute {title} for TITLE and {date} for DATE in TEMPLATE-STRING."
  (declare (side-effect-free t))
  ;; TODO There has to be a more elegant way of doing this:
  (let* ((title-without-backslash (replace-regexp-in-string "\\\\" "\\\\\\\\" title))
         (substituted-title
          (replace-regexp-in-string "{title}" title-without-backslash template-string))
         (substituted-date
          (replace-regexp-in-string "{date}" date substituted-title))
         (substituted-timestamp
          (replace-regexp-in-string "{timestamp}" timestamp substituted-date)))
    substituted-timestamp))


(defun mvtn-list-files-function-native (&optional search)
  "Native (elisp) implementation for `mvtn-list-files-function'.
Does not show hidden files (prefixed with '.').  Result may
optionally be limited to only items matching SEARCH."
  (mapcar (lambda (file-name)
            (substring file-name 2))
          (sort (directory-files-recursively
                 "." (if search (format "^[^\\.]*%s" search) "^[^\\.]") nil
                 (lambda (dir-name)
                   (not (member (file-name-nondirectory dir-name)
                                mvtn-excluded-directories)))) 'string<)))


(defun mvtn-list-files-function-find (&optional search)
  "GNU/POSIX find implementation for `mvtn-list-files-function'.
Requires GNU sort command to return persistent, name based sorting
since find's sorting relies on creation time.  Result may
optionally be limited to only items matching SEARCH."
  (split-string
   (shell-command-to-string
    (format "%s * -type f %s -print %s | sort"
            find-program
            (if search
                (format "-name '*%s*'" search) "")
            (if mvtn-excluded-directories
                (format "-o -path '*%s' -type d -prune"
                        (mapconcat 'identity mvtn-excluded-directories
                                   "' -type d -prune -o -path '*")) "")))
   "\n" t))


(defun mvtn--directory-files (dir &optional prefix search)
  "Helper function for running `mvtn-list-files-function'.
Checks if DIR exists, calls `mvtn-list-files-function' with
`default-directory' set to DIR and prefixes all results with
PREFIX.  Result may optionally be limited to only items matching
SEARCH."
  (if (file-exists-p dir)
      (let ((default-directory dir))
        (if prefix
            (mapcar (lambda (el) (format "%s/%s" prefix el))
                    (funcall mvtn-list-files-function search))
          (funcall mvtn-list-files-function search)))
    nil))

(defun mvtn-list-files (&optional all)
  "Return a list of all notes in `mvtn-note-directories'.
Limit to `mvtn-search-years' unless ALL is non-nil."
  (let* ((files-datetree '()) (files-other '())
         (current-year (string-to-number (format-time-string "%Y"))))
    ;; datetree first
    (dolist (root-el mvtn-note-directories)
      (dolist (structure-el (plist-get root-el :structure))
        (let* ((structure-dir (plist-get structure-el :dir))
               (root-name (plist-get root-el :name))
               (root-dir (plist-get root-el :dir)))
          (if (and (plist-get structure-el :datetree)
                   (file-exists-p (format "%s/%s" root-dir structure-dir)))
              (dolist (year (if all
                                (directory-files (format "%s/%s" root-dir structure-dir)
                                                 nil "^[[:digit:]]\\{4\\}$")
                              (number-sequence
                               (1+ (- current-year mvtn-search-years)) current-year)))
                (let ((dir (format "%s/%s/%s/" root-dir structure-dir year))
                      (prefix (format "%s/%s/%s/" root-name structure-dir year)))
                  (if (file-exists-p dir)
                      (setq files-datetree (append (mapcar (lambda (el) (cons el prefix))
                                                     (mvtn--directory-files dir))
                                             files-datetree)))))))))
    (setq files-datetree (sort files-datetree (lambda (a b) (string< (car a) (car b)))))
    ;; non-datetree second
    (dolist (root-el mvtn-note-directories)
      (dolist (structure-el (plist-get root-el :structure))
        (let* ((structure-dir (plist-get structure-el :dir))
               (root-name (plist-get root-el :name))
               (root-dir (plist-get root-el :dir)))
          (if (not (plist-get structure-el :datetree))
            (let ((dir (format "%s/%s/" root-dir structure-dir))
                  (prefix (format "%s/%s/" root-name structure-dir)))
              (if (file-exists-p dir)
                  (setq files-other (append (mapcar (lambda (el) (cons el prefix))
                                                 (mvtn--directory-files dir))
                                         files-other))))))))
    (setq files-other
          (sort files-other (lambda (a b) (string< (mvtn--extract-note-identity (car a))
                                              (mvtn--extract-note-identity (car b))))))
    (when (eq mvtn-list-files-order 'desc)
      (setq files-datetree (reverse files-datetree))
      (setq files-other (reverse files-other)))
    (mapcar (lambda (el) (concat (cdr el) (car el)))
            (append files-datetree files-other))))


(defun mvtn-title-to-acceptable-file-name (title)
  "RETURN a reasonably sanitized form of TITLE.
All characters not allowed on either NTFS or EXT4 will be
replaced by an underscore ('_')

Specifically:
- / is not allowed on Linux
- <, >, :, \", /, \\, |, ? and * are not allowed on Windows"
  (replace-regexp-in-string "/\\|<\\|>\\|*\\||\\|:\\|\\\\\\|?" "_" title))


(defun mvtn-generate-file-name (timestamp title extension tags &optional encrypt)
  "Get an mvtn file-name following this template:
\"{TIMESTAMP} {TITLE} -- {TAGS}.{EXTENSION}[.gpg]\"
TAGS is a list of strings, TIMESTAMP, TITLE, EXTENSION are strings. ENCRYPT is
either nil or non-nil."
  (substring-no-properties
   (format "%s %s%s.%s%s" timestamp (mvtn-title-to-acceptable-file-name title)
           (if tags (concat " -- " (mapconcat 'identity tags " ")) "")
           extension (if encrypt ".gpg" ""))))

(defun mvtn-touch-new-file (dir timestamp title extension tags &optional encrypt)
  "Use `mvtn-generate-file-name' to create a new file in DIR.
DIR must be one of `mvtn-short-note-dir-list'.  If DIR is
configured as a datetree, the file is created in the directory
for the current year.  RETURN the full name of the newly created
file.  TIMESTAMP, TITLE, EXTENSION, TAGS and ENCRYPT will all be
passed to `mvtn-generate-file-name'."
  (let* ((file-name (mvtn-generate-file-name timestamp title extension tags encrypt))
         (full-dir (concat (mvtn-expand-note-name dir)
                           (if (member dir (mvtn-short-note-dir-list t))
                               (concat "/" (format-time-string "%Y")) "")))
         (default-directory full-dir))
    (if (not (file-exists-p full-dir)) (mkdir full-dir t))
    (write-region "" nil file-name)
    (substring-no-properties (concat full-dir "/" file-name))))

(defun mvtn-create-new-file (timestamp dir title tags content &optional encrypt)
  "Use `mvtn-touch-new-file' to create a new file.
After file creation, CONTENT is inserted and a buffer to the
resulting file is opened.  TIMESTAMP, DIR, TITLE, TAGS and
ENCRYPT will all be passed to `mvtn-touch-new-file'.  RETURN the
buffer to the new file.  When {point} is found in the buffer,
place point there before returning."
  (let* ((file-name (mvtn-touch-new-file
                     dir timestamp title mvtn-default-file-extension tags encrypt))
         (buf (find-file-noselect file-name)))
    (with-current-buffer buf (insert content) (save-buffer))
    ;; When creating an encrypting a file with 'epa.el', the user is prompted
    ;; for the gpg key to use everytime the buffer is saved. Once the file is
    ;; closed and reopened though, epa seems to remember the key to use.
    (when encrypt (kill-buffer buf) (setq buf (find-file-noselect file-name)))
    ;; When {point} is found in content, place point in buffer there
    (when (string-match-p "{point}" content)
      (with-current-buffer buf (goto-char (point-min)) (search-forward "{point}")
                           (backward-delete-char 7) (insert " ") (save-buffer)))
    buf))


(defun mvtn--extract-note-identity (notename &optional filename)
  "Extracts the timestamp from NOTENAME.
When FILENAME is given, also extracts the filename without
extension and tags"
  (unless (string-match (if filename (concat mvtn--id-regexp ".*") mvtn--id-regexp)
                        notename)
    (error (concat "Failed to extract note identity. "
                   "Probably an invalid filename or timestamp: %s")
           notename))
  (let* ((match (match-string-no-properties 0 notename))
         (sep (cond
               ((string-match-p "--" match) "--") ; tags
               ((string-match-p "\\.[[:alpha:]]+\\^\\^" match) "\\.[[:alpha:]]+\\^\\^")
               ((string-match-p "\\^\\^" match) "\\^\\^") ; links
               ((string-match-p "\\.[[:alpha:]]+$" match) "\\.[[:alpha:]]+$") ; extension
               )))
    (string-trim (car (split-string match sep)))))


(defun mvtn-link-targets (link)
  "Determine the target file of the given LINK.
The only relevant part of a link for determining this target is
the id aka timestamp.  Other parts of the link are ignored.  This
is to allow renaming of files even without automated tooling.  As
long as the timestamp of the target file is untouched, links to
it will not break.

Returns a *list* of targets matching the timestamp in order to
catch synchronisation conflicts of some tools such as nextcloud
or syncthing.

Example:
\(mvtn-link-targets \"^^20210110-000548 ABCDEFGBLABLA.asd^^\")
-> \"prv/devlog/2021/20210110-000548 Branching in Subversion.org\""
  (when (not (string-match-p mvtn--link-regexp link)) (error "Invalid mvtn link"))
  (let* ((timestamp (mvtn--extract-note-identity link))
         (year (mvtn-timestamp-field timestamp 'year))
         (matches '()))
    ;; datetrees first
    (dolist (root-el mvtn-note-directories)
      (dolist (structure-el (plist-get root-el :structure))
        (let ((root-dir (plist-get root-el :dir))
              (root-name (plist-get root-el :name))
              (structure-dir (plist-get structure-el :dir)))
          (if (plist-get structure-el :datetree)
              (let ((dir (format "%s/%s/%s" root-dir structure-dir year))
                    (prefix (format "%s/%s/%s" root-name structure-dir year)))
                (setq matches (append matches (mvtn--directory-files
                                               dir prefix timestamp))))))))
    ;; non-datetrees second
    (dolist (root-el mvtn-note-directories)
      (dolist (structure-el (plist-get root-el :structure))
        (let ((root-dir (plist-get root-el :dir))
              (root-name (plist-get root-el :name))
              (structure-dir (plist-get structure-el :dir)))
          (if (not (plist-get structure-el :datetree))
              (let ((dir (format "%s/%s" root-dir structure-dir))
                    (prefix (format "%s/%s" root-name structure-dir)))
                (setq matches (append matches (mvtn--directory-files
                                               dir prefix timestamp))))))))
    matches))


(defun mvtn-follow-link (link)
  "Follows the mvtn link LINK.
If multiple matches exists,prompts for disambiguation."
  (when (not (string-match-p (format "^%s$" mvtn--link-regexp) link))
    (error "Not a valid mvtn link: %s" link))
  (let* ((matches (mvtn-link-targets link))
         (target (cond ((> (length matches) 1)
                        (completing-read "Pick match: " matches))
                       ((= (length matches) 1)
                        (car matches))
                       (t (error "No matches found for link")))))
    (find-file (mvtn-expand-note-name target))
    (dolist (el mvtn-link-actions)
      (when (string-match-p (car el) link)
        (funcall (cadr el)
                 (substring (nth 1 (split-string link (car el))) 0 -2))))))


(defun mvtn-link-action-search (search)
  "Go to the first occurence of SEARCH in current buffer.
Also highlight the match with `pulse', if available.  See
`mvtn-link-actions'."
  (goto-char (point-min)) (search-forward search)
  (goto-char (match-beginning 0))
  (require 'pulse nil nil)
  (when (fboundp 'pulse-momentary-highlight-region)
    (pulse-momentary-highlight-region (match-beginning 0) (match-end 0))))


(defun mvtn-search-full-text-grep (string dirs)
  "Search for STRING using `grep' in DIRS."
  ;; `grep-use-null-device' is only useful when -H is not an option and only one
  ;; file is searched. Although -H is apparently not posix-compliant, it is
  ;; included in both BSD and GNU grep. I therefore hereby declare that this
  ;; function shall only work with those implementations of grep. When
  ;; `grep-use-null-device' remains enabled, it causes problems when using
  ;; compatibility layers like cygwin.
  (let ((grep-use-null-device nil))
    (grep (concat (shell-quote-argument (executable-find "grep"))
                  " "
                  (shell-quote-argument string)
                  " -nH --null -r -I "
                  (mapconcat 'identity dirs " ")))))


(defun mvtn--search-dirs (&optional all)
  "RETURN a list of all directories for search.
See `mvtn-search-years' and `mvtn-excluded-directories' for why
this is not necessarily every directory in
`mvtn-note-directories'.  Paths for the subdirectories of the
first element in `mvtn-note-directories' are returned as relative
paths.  When ALL is non-nil, `mvtn-search-years' is ignored."
  (let ((result '())
        (current-year (string-to-number (format-time-string "%Y")))
        (mvtn-search-years (if all 1000 mvtn-search-years)))
    (dolist (root-el mvtn-note-directories)
      (dolist (currstruct (plist-get root-el :structure))
        (when (file-exists-p (concat (plist-get root-el :dir) "/"
                                     (plist-get currstruct :dir)))
          (if (plist-get currstruct :datetree)
            (let* ((currdir (plist-get currstruct :dir))
                   (yeardirs (directory-files
                              (concat (plist-get root-el :dir) "/"
                                      currdir)
                              nil "^[[:digit:]]\\{4\\}$"))
                   (yeardirs-filtered
                    (seq-filter (lambda (dir) (> dir (- current-year mvtn-search-years)))
                                (mapcar 'string-to-number yeardirs)))
                   (prefix (if (not (eq root-el (car mvtn-note-directories)))
                               (concat (expand-file-name (plist-get root-el :dir)) "/") ""))
                   (searchdirs
                    (mapcar (lambda (el) (concat prefix currdir "/" (number-to-string el)))
                            yeardirs-filtered)))
              (setq result (append result searchdirs)))
          (let ((prefix (if (not (eq root-el (car mvtn-note-directories)))
                            (concat (expand-file-name (plist-get root-el :dir)) "/") "")))
            (setq result
                  (append result (list (concat
                                        prefix (plist-get currstruct :dir))))))))))
    (seq-filter 'file-exists-p result)))


;;;###autoload
(defun mvtn-search-full-text (string &optional all)
  "Search for STRING in `mvtn-note-directories'.
If ALL is non-nil, `mvtn-search-years' will be ignored."
  (interactive "MSearch: \nP")
  (let* ((default-directory (plist-get (car mvtn-note-directories) :dir)))
    (if all
        (funcall mvtn-search-function string (mvtn--search-dirs t))
      (funcall mvtn-search-function string (mvtn--search-dirs)))))


;;;###autoload
(defun mvtn-search-backlinks (&optional all file)
  "Search for backlinks to the given FILE.
FILE defaults to the file opened in the current buffer.  If ALL
is non-nil (can be set through a universal argument), then
`mvtn-search-years' is ignored."
  (interactive "P")
  (when (not file) (setq file (file-name-nondirectory buffer-file-name)))
  (when (not (string-match-p (concat "^" mvtn--id-regexp ".*") file))
    (error "Invalid mvtn filename.  Cancelled backlink search"))
  (let ((timestamp (mvtn--extract-note-identity file)))
    (mvtn-search-full-text (concat "\\^\\^" timestamp) all)))


;;;###autoload
(defun mvtn-follow-link-at-point ()
  "Follow the mvtn link under point."
  (interactive)
  (save-excursion
    ;; Move to the beginning of the link
    (cond ((looking-at "\\^\\^[[:digit:]]") nil)
          ((looking-at "\\^[[:digit:]]") (backward-char))
          (t (search-backward "^^" (point-at-bol) t)))
    (if (looking-at mvtn--link-regexp)
        (mvtn-follow-link (buffer-substring-no-properties
                           (match-beginning 0) (match-end 0)))
      (error "No link under point"))))


;;;###autoload
(defun mvtn-insert-link ()
  "Prompt for a note to insert a link to."
  (interactive)
  (let* ((answer (completing-read "Insert link to: " (mvtn-list-files)))
         (link (mvtn--extract-note-identity answer t)))
    (when (not (string-match-p (concat "^" mvtn--id-regexp ".*") link))
      (error "Invalid mvtn filename: %s" answer))
    (insert (format "^^%s^^" link))))


;;;###autoload
(defun mvtn-jump-current-year-directory ()
  "Jump to a directory in `mvtn-note-directories'.
Opens the directory for the current year if a directory
configured as a datetree is selected."
  (interactive)
  (let ((choice (completing-read "Directory: " (mvtn-short-note-dir-list))))
    (when (member choice (mvtn-short-note-dir-list t))
      (setq choice (concat choice "/" (format-time-string "%Y"))))
    (setq choice (mvtn-expand-note-name choice))
    (if (not (file-exists-p choice)) (mkdir choice t))
    (dired choice)))

(defun mvtn-prompt-for-tags ()
  "Prompt for a selection of comma-separated tags."
  (let ((answer (read-from-minibuffer "Tags (comma-seperated): ")))
    (if (eq (length answer) 0) nil (split-string answer ","))))

;;;###autoload
(defun mvtn-new-note (&optional encrypt)
  "Create a new note using `mvtn-create-new-file'.
Switch to the buffer of the new note.  If ENCRYPT is non-nil,
'epa.el' is used to encrypt the file with gpg."
  (interactive "P")
  (let* ((dir (completing-read "Directory: " (mvtn-short-note-dir-list)))
         (timestamp (mvtn-current-timestamp 'second))
         (title (read-from-minibuffer "Title: "))
         (content (mvtn-substitute-template
                   (mvtn-template-for-extension mvtn-default-file-extension)
                   title (format-time-string "%Y-%m-%d") timestamp))
         (tags (if mvtn-cv-enable
                   (mvtn-cv-prompt-for-tags) (mvtn-prompt-for-tags))))
    (switch-to-buffer (mvtn-create-new-file timestamp dir title tags content encrypt))))


;;;###autoload
(defun mvtn-open-note (&optional all)
  "Open a note from `mvtn-note-directories'.
If ALL is non-nil, ignore `mvtn-search-years'."
  (interactive "P")
  (let* ((answer (completing-read "Open note: " (mvtn-list-files all))))
    (find-file (mvtn-expand-note-name answer))))

(defvar mvtn-minor-mode-map (make-sparse-keymap))
(define-key mvtn-minor-mode-map (kbd "C-c C-. o") 'mvtn-follow-link-at-point)
(define-key mvtn-minor-mode-map (kbd "C-c C-. i") 'mvtn-insert-link)
(define-key mvtn-minor-mode-map (kbd "C-c C-. b") 'mvtn-search-backlinks)
(define-key mvtn-minor-mode-map (kbd "C-c C-. r") 'mvtn-rename-current-file)
(defvar mvtn-global-map (make-sparse-keymap))
(define-key mvtn-global-map (kbd "n") 'mvtn-new-note)
(define-key mvtn-global-map (kbd "o") 'mvtn-open-note)
(define-key mvtn-global-map (kbd "s") 'mvtn-search-full-text)
(define-key mvtn-global-map (kbd "j") 'mvtn-jump-current-year-directory)
(define-key mvtn-global-map (kbd "t") 'mvtn-tag-file-list)
(global-set-key (kbd "C-x C-.") mvtn-global-map)

(define-minor-mode mvtn-minor-mode "A minor mode for editing mvtn notes"
  nil " mvtn" mvtn-minor-mode-map)

(defun maybe-enable-mvtn-minor-mode ()
  "Enable `mvtn-minor-mode' when file is in `mvtn-note-directories'."
  (condition-case nil
      (let* ((note-dirs (mapcar 'expand-file-name
                                (mapcar 'mvtn-expand-note-name
                                        (mvtn-short-note-dir-list))))
             (matches (mapcar (lambda (el) (string-match-p
                                       el (buffer-file-name
                                           (current-buffer))))
                              note-dirs)))
        (when (> (length (remq nil matches)) 0)
               (mvtn-minor-mode 1)))
     (error nil)))

(add-hook 'text-mode-hook 'maybe-enable-mvtn-minor-mode)

(when (< emacs-major-version 27) (require 'mvtn-compat))

(provide 'mvtn)

;;; mvtn.el ends here
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

(defcustom mvtn-note-directory (expand-file-name "~/mvtn")
  "The base directory for all your mvtn notes."
  :type 'string :group 'mvtn)

;; TODO: add documentation
(setq mvtn-note-directories
      '((:dir "~/sync/documents/notes/mvtn" :name "default" :main t :structure
              ((:dir "fleeting" :datetree t)
               (:dir "zettelkasten" :datetree t)
               (:dir "devlog" :datetree t)
               (:dir "static" :datetree nil)))
        (:dir "d:/mvtn-test" :name "work" :main t :structure
              ((:dir "fleeting" :datetree t)
               (:dir "meetings" :datetree t)
               (:dir "static" :datetree nil)))))

(defcustom mvtn-static-note-directories
  '("static")
  "A list containing all the directory names inside the
`mvtn-note-directory' that contain notes which are year
independent. These notes are always displayed when calling
`mvtn-open-note', `mvtn-insert-link' and everything else affected
by `mvtn-list-files-function' - independently from the value of
`mvtn-search-years'."
  :type '(list :value-type string) :group 'mvtn)

(defcustom mvtn-excluded-directories
  '(".git" ".svn" "ltximg")
  "A list containing all directory names that should be ignored
when calling `mvtn-open-note', `mvtn-insert-link' and everything
else affected by `mvtn-list-files-function'. These directories
might contain linked pictures, LaTeX fragments or anything that
is not a note (NaN)."
  :type '(list :value-type string) :group 'mvtn)

(defcustom mvtn-default-file-extension "org"
  "The default extension of new mvtn notes. Any extension is
allowed, as mvtn is not dependant on org-mode or markdown
features, but it will be created and treated as a plaintext
file."
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
  "An associative list of templates for new files by file
extension in mvtn. When an extension is not matched, the fallback
associated with the empty string (\"\") is used.

Available substitutions:
{title}    : The notes title
{date}     : The notes date of creation
{timestamp}: The timestamp/id of the note

The first lines of these templates should not be changed, as they
are required for some functions such as
`mvtn-rename-current-file'."
  :type '(alist :value-type (group string)) :group 'mvtn)

(defcustom mvtn-list-files-order 'desc
  "Either 'asc or 'desc. Affects `mvtn-open-note',
`mvtn-insert-link' and everything else calling
`mvtn-list-files-function'."
  :type 'symbol :group 'mvtn)

(defcustom mvtn-search-years 3
  "Search in mvtn is by default limited to the previous n
years (including the current year). This is done for long-term
scalability."
  :type 'number :group 'mvtn)

(defcustom mvtn-search-function 'mvtn-search-full-text-grep
  "The function used in mvtn search commands
`mvtn-search-full-text'. Its first argument should be the string
to search for and the second argument a list of directories (as
strings) to exclude from the search. By default,
`mvtn-search-full-text-grep' is used."
  :type 'symbol :group 'mvtn)

(defcustom mvtn-list-files-function
  (if (and (not (eq system-type 'windows-nt))
           (executable-find find-program))
      'mvtn-list-files-function-find
    'mvtn-list-files-function-native)
  "Function to traverse all directories (year dependent &
`mvtn-static-note-directories') in `mvtn-note-directory'
excluding directories provided in `mvtn-excluded-directories'.

Takes one optional argument SEARCH which allows to only list
files matching this specific string

RETURN a list of all files (notes).

See `mvtn-list-files-function-native' and `mvtn-list-files-function-find'."
  :type 'symbol :group 'mvtn)

(defcustom mvtn-link-actions '((" :: " mvtn-link-action-search))
  "Links may specify additional \"actions\" to be executed after
following the link. These actions are defined in this alist. The
car of each element of this list is interpreted as a
seperator. If this seperator (regexp) matches a link, then
everything following the seperator will be passed to its
associated function *after* following the link. (Mvtn goes
through this list top to bottom and only executes the first
applicable action.)"
  :type '(alist :value-type (group symbol)) :group 'mvtn)

(defvar mvtn--id-regexp "[[:digit:]]\\{8\\}-[[:digit:]]\\{6\\}"
  "A regexp matching a valid mvtn id.")
(defvar mvtn--link-regexp (concat "\\^\\^" mvtn--id-regexp ".*\\^\\^")
  "A regexp matching valid mvtn links.")

(defun mvtn-note-dir-for-name (name)
  "Returns the :path property of a note directory in
`mvtn-note-directories' associated with the given NAME"
  (let ((candidates (seq-filter (lambda (el) (string-equal (plist-get el :name) name))
                                mvtn-note-directories)))
    (cond
     ((not candidates) (error "No note directory found for name: %s" name))
     ((> (length candidates) 1) (error "Conflicting note directory names: %s" name))
     (t (plist-get (car candidates) :dir)))))

(defun mvtn-expand-note-name (notename)
  "Translates a path to a note shortened by the :name property of
the note directory in `mvtn-note-directories' to the full path on
the disk."
  (let ((split (split-string notename "/")))
    (concat (mvtn-note-dir-for-name (car split)) "/"
            (mapconcat 'identity (cdr split) "/"))))

(defun mvtn-current-timestamp (accuracy)
  "Returns a timestamp for use in generating mvtn filenames. ACCURACY is a
symbol to define the format:
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
  "Returns a field in an mvtn timestamp like this:
\"{year}{month}{day}[-{hour}[{minute}[{second}]]]\". FIELD may be
one of 'year, 'month, 'day, 'hour, 'minute or 'second."
  (declare (side-effect-free t))
  (cond ((eq field 'year) (substring timestamp 0 4))
        ((eq field 'month) (substring timestamp 4 6))
        ((eq field 'day) (substring timestamp 6 8))
        ((and (eq field 'hour) (>= (length timestamp) 11)) (substring timestamp 9 11))
        ((and (eq field 'minute) (>= (length timestamp) 13)) (substring timestamp 11 13))
        ((and (eq field 'second) (>= (length timestamp) 15)) (substring timestamp 13 15))
        (t nil)))


(defun mvtn-template-for-extension (extension)
  "Return the template in `mvtn-file-extension-templates' for EXTENSION."
  (declare (side-effect-free t))
  (or (cadr (assoc extension mvtn-file-extension-templates))
      (cadr (assoc "" mvtn-file-extension-templates))))


(defun mvtn-substitute-template (template-string title date timestamp)
  "Substitute {title} for TITLE and {date} for DATE in TEMPLATE-STRING."
  (declare (side-effect-free t))
  ;; TODO There has to be a more elegant way of doing this:
  (let* ((substituted-title
          (replace-regexp-in-string "\{title\}" title template-string))
         (substituted-date
          (replace-regexp-in-string "\{date\}" date substituted-title))
         (substituted-timestamp
          (replace-regexp-in-string "\{timestamp\}" timestamp substituted-date)))
    substituted-timestamp))


(defun mvtn-get-create-current-year-directory ()
  "Returns a directory with the name of the current year in
`mvtn-note-directory'. If it does not yet exist, it is created."
  (let ((dir-name (format "%s/%s" mvtn-note-directory (format-time-string "%Y"))))
    (when (not (file-exists-p dir-name))
      (mkdir dir-name t))
    dir-name))


(defun mvtn-list-files-function-native (&optional search)
  "Native (elisp) implementation for `mvtn-list-files-function'.
Does not show hidden files (prefixed with '.')"
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
since find's sorting relies on creation time"
  (split-string
   (shell-command-to-string
    (format "%s * -type f %s -print %s | sort"
            (executable-find find-program)
            (if search
                (format "-name '*%s*'" search) "")
            (if mvtn-excluded-directories
                (format "-o -path '*%s' -type d -prune"
                        (mapconcat 'identity mvtn-excluded-directories
                                   "' -type d -prune -o -path '*")) "")))
   "\n" t))


(defun mvtn--directory-files (dir &optional prefix search)
  "Checks if DIR exists, calls `mvtn-list-files-function' with
`default-directory' set to DIR and prefixes all results with PREFIX."
  (if (file-exists-p dir)
      (let ((default-directory dir))
        (if prefix
            (mapcar (lambda (el) (format "%s/%s" prefix el))
                    (funcall mvtn-list-files-function search))
          (funcall mvtn-list-files-function search)))
    nil))

(defun mvtn-list-files (&optional all)
  "Return a list of all files in `mvtn-note-directory'
recursively. Limit to `mvtn-search-years' unless ALL is non-nil."
  (let* ((files-datetree '())
         (files-other '())
         (current-year (string-to-number (format-time-string "%Y")))
         (yearlist (if all
                       (directory-files mvtn-note-directory nil "^[[:digit:]]\\{4\\}$")
                     (number-sequence (1+ (- current-year mvtn-search-years)) current-year))))
    ;; datetree first
    (dolist (root-el mvtn-note-directories)
      (dolist (structure-el (plist-get root-el :structure))
        (let* ((datetree (plist-get structure-el :datetree))
               (structure-dir (plist-get structure-el :dir))
               (root-name (plist-get root-el :name))
               (root-dir (plist-get root-el :dir)))
          (if datetree
              (dolist (year yearlist)
                (let ((dir (format "%s/%s/%s/" root-dir structure-dir year))
                      (prefix (format "%s/%s/%s/" root-name structure-dir year)))
                  (if (file-exists-p dir)
                      (setq files-datetree (append (mapcar (lambda (el) (cons el prefix))
                                                     (mvtn--directory-files dir))
                                             files-datetree)))))))))
    (setq files-datetree
          (mapcar (lambda (el) (concat (cdr el) (car el)))
                  (sort files-datetree (lambda (a b) (string< (car a) (car b))))))
    ;; non-datetree second
    (dolist (root-el mvtn-note-directories)
      (dolist (structure-el (plist-get root-el :structure))
        (let* ((datetree (plist-get structure-el :datetree))
               (structure-dir (plist-get structure-el :dir))
               (root-name (plist-get root-el :name))
               (root-dir (plist-get root-el :dir)))
          (if (not datetree)
            (let ((dir (format "%s/%s/" root-dir structure-dir))
                  (prefix (format "%s/%s/" root-name structure-dir)))
              (if (file-exists-p dir)
                  (setq files-other (append (mapcar (lambda (el) (cons el prefix))
                                                 (mvtn--directory-files dir))
                                         files-other))))))))
    (setq files-other
          (mapcar (lambda (el) (concat (cdr el) (car el)))
                  (sort files-other
                        (lambda (a b) (string< (mvtn--extract-note-identity (car a))
                                          (mvtn--extract-note-identity (car b)))))))
    (when (eq mvtn-list-files-order 'desc)
      (setq files-datetree (reverse files-datetree))
      (setq files-other (reverse files-other)))
    (setq filelist (append files-datetree files-other))))

(defun mvtn-generate-file-name (timestamp title extension tags &optional encrypt)
  "Get an mvtn file-name following this template:
\"{TIMESTAMP} {TITLE} -- {TAGS}.{EXTENSION}[.gpg]\""
  (substring-no-properties
   (format "%s/%s %s%s.%s%s" (mvtn-get-create-current-year-directory)
           timestamp title (if (car (split-string tags)) (concat " -- " tags) "")
           extension (if encrypt ".gpg" ""))))

(defun mvtn-touch-new-file (timestamp title extension tags &optional encrypt)
  "Use `mvtn-generate-file-name' to create a new file.
RETURN the full name of the newly created file."
  (let ((file-name (mvtn-generate-file-name timestamp title extension tags encrypt)))
    (write-region "" nil file-name) file-name))

(defun mvtn-create-new-file (title tags &optional encrypt no-template)
  "Use `mvtn-touch-new-file' to create a new file, insert a
template according to `mvtn-file-extension-templates' and open
the buffer to the resulting file. RETURN that buffer."
  (let* ((timestamp (mvtn-current-timestamp 'second))
         (file-name (mvtn-touch-new-file
                     timestamp title mvtn-default-file-extension tags encrypt))
         (template (mvtn-substitute-template
                    (mvtn-template-for-extension mvtn-default-file-extension)
                    title (format-time-string "%Y-%m-%d") timestamp))
         (buf (find-file-noselect file-name)))
    (when (not no-template) (with-current-buffer buf (insert template) (save-buffer)))
    ;; When creating an encrypting a file with 'epa.el', the user is prompted
    ;; for the gpg key to use everytime the buffer is saved. Once the file is
    ;; closed and reopened though, epa seems to remember the key to use.
    (when encrypt (kill-buffer buf) (setq buf (find-file-noselect file-name)))
    buf))


(defun mvtn--extract-note-identity (notename &optional filename)
  "Extracts the timestamp from NOTENAME (any string representation
of a note's name). When FILENAME is given, also extracts the
filename without filextension and tags"
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
  "Determine the target file of the given LINK. The only relevant
part of a link for determining this target is the id aka
timestamp. Other parts of the link are ignored. This is to allow
renaming of files even without automated tooling. As long as the
timestamp of the target file is untouched, links to it will not
break.

Returns a *list* of targets matching the timestamp in order to
catch synchronisation conflicts of some tools such as nextcloud
or syncthing.

Example:
(mvtn-link-targets \"^^20210110-000548 ABCDEFGBLABLA.asd^^\")
-> \"/path/to/notes/2021/20210110-000548 Branching in Subversion.org\""
  (when (not (string-match-p mvtn--link-regexp link)) (error "Invalid mvtn link"))
  (let* ((timestamp (mvtn--extract-note-identity link))
         (year-dir (mvtn-timestamp-field timestamp 'year))
         (matches '()))
    (dolist (current-dir `(,year-dir ,@mvtn-static-note-directories))
      (let ((default-directory (format "%s/%s" mvtn-note-directory current-dir)))
        (setq matches (append matches
                              (mapcar (lambda (filename)
                                        (format "%s/%s" current-dir filename))
                                      (mvtn--directory-files timestamp))))))
    matches))


(defun mvtn-follow-link (link)
  "Follows the mvtn link LINK. If multiple matches exists,
prompts for disambiguation."
  (when (not (string-match-p (format "^%s$" mvtn--link-regexp) link))
    (error "Not a valid mvtn link: %s" link))
  (let* ((matches (mvtn-link-targets link))
         (target (cond ((> (length matches) 1)
                        (completing-read "Pick match: " matches))
                       ((= (length matches) 1)
                        (car matches))
                       (t (error "No matches found for link")))))
    (find-file (format "%s/%s" mvtn-note-directory target))
    (dolist (el mvtn-link-actions)
      (when (string-match-p (car el) link)
        (funcall (cadr el)
                 (substring (nth 1 (split-string link (car el))) 0 -2))))))


(defun mvtn-link-action-search (search)
  "Go to the beginning of the first occurence of SEARCH in
current buffer and highlight the match with `pulse', if
available. See `mvtn-link-actions'."
  (goto-char (point-min)) (search-forward search)
  (goto-char (match-beginning 0))
  (require 'pulse nil nil)
  (when (fboundp 'pulse-momentary-highlight-region)
    (pulse-momentary-highlight-region (match-beginning 0) (match-end 0))))


(defun mvtn-search-full-text-grep (string exclude-dirs)
  "Searches STRING using `grep' in `default-directory', excluding
directories specified as a list of strings in DIRS."
  ;; `grep-use-null-device' is only useful when -H is not an option and only one
  ;; file is searched. Although -H is apparently not posix-compliant, it is
  ;; included in both BSD and GNU grep. I therefore hereby declare that this
  ;; function shall only work with those implementations of grep. When
  ;; `grep-use-null-device' remains enabled, it causes problems when using
  ;; compatibility layers like cygwin.
  (let ((grep-use-null-device nil))
    (grep (concat (executable-find "grep") " -nH --null -r -I "
                  (mapconcat (lambda (dir) (format "--exclude-dir=\"%s\"" dir))
                             exclude-dirs " ")
                  " " (shell-quote-argument string)))))


;;;###autoload
(defun mvtn-search-full-text (string &optional all)
  "Search for STRING in `mvtn-note-directory', using
`mvtn-search-function', excluding directories according to
`mvtn-search-years'."
  (interactive "MSearch: \nP")
  (let* ((default-directory mvtn-note-directory)
         (current-year (string-to-number (format-time-string "%Y")))
         (exclude-dirs
          (seq-filter (lambda (dir) (<= dir (- current-year mvtn-search-years)))
                      (mapcar 'string-to-number
                              (directory-files mvtn-note-directory nil
                                               "^[[:digit:]]\\{4\\}$")))))
    (if all
        (funcall mvtn-search-function string nil)
      (funcall mvtn-search-function string exclude-dirs))))


;;;###autoload
(defun mvtn-search-backlinks (&optional all file)
  "Uses `mvtn-search-full-text' to look for backlinks to the
given FILE (defaults to the file opened in the current
buffer). If ALL is non-nil (can be set through a universal
argument), then `mvtn-search-years' is ignored."
  (interactive "P")
  (when (not file) (setq file (file-name-nondirectory buffer-file-name)))
  (when (not (string-match-p (concat "^" mvtn--id-regexp ".*") file))
    (error "Invalid mvtn filename. Cancelled backlink search."))
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
  "Prompt for a note to insert a link to. Supports completion."
  (interactive)
  (let* ((answer (completing-read "Insert link to: " (mvtn-list-files)))
         (link (mvtn--extract-note-identity answer t)))
    (when (not (string-match-p (concat "^" mvtn--id-regexp ".*") link))
      (error "Invalid mvtn filename: %s" answer))
    (insert (format "^^%s^^" link))))


;;;###autoload
(defun mvtn-jump-current-year-directory ()
  "Jump to {`mvtn-note-directory'}/{current-year}. Uses `dired'
or whatever `find-file' is configured to do for directories."
  (interactive)
  (find-file (mvtn-get-create-current-year-directory)))


;;;###autoload
(defun mvtn-new-note (&optional encrypt)
  "Creates a new note using `mvtn-create-new-file'. Switches to
the buffer of the new note. If ENCRYPT is non-nil, 'epa.el' is
used to encrypt the file with gpg."
  (interactive "P")
  (let ((title (read-from-minibuffer "Title: "))
        (tags (read-from-minibuffer "Tags: ")))
    (switch-to-buffer (mvtn-create-new-file title tags encrypt))))


;;;###autoload
(defun mvtn-open-note (&optional all)
  "Opens a note from `mvtn-note-directory'. Supports completion."
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
  "Enable `mvtn-minor-mode' when file is in `mvtn-note-directory'"
    (condition-case nil
        (when (string-match-p (expand-file-name mvtn-note-directory)
                              (buffer-file-name (current-buffer)))
          (mvtn-minor-mode 1)) (error nil)))

(add-hook 'text-mode-hook 'maybe-enable-mvtn-minor-mode)

(when (< emacs-major-version 27) (require 'mvtn-compat))

(provide 'mvtn)

;;; mvtn.el ends here
;;; mvtn.el --- Minimum Viable Text Notes -*- lexical-binding: t -*-

;; Author: f1p
;; Maintainer: f1p
;; Version: 0.1
;; Package-Requires: ()
;; Homepage: https://github.com/f1rstperson/mvtn
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


(defcustom mvtn-note-directory (expand-file-name "~/mvtn")
  "The base directory for all your mvtn notes."
  :type 'string :group 'mvtn)

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
    ("" "title          : {title}
date :: {date}
original_title :: {title}
original_id :: {timestamp}
----------------------------------------------------------------------\n\n"))
  "An associative list of templates for new files by file
extension in mvtn. When an extension is not matched, the fallback
associated with the empty string (\"\") is used.

Available substitutions:
{title}    : The notes title
{date}     : The notes date of creation
{timestamp}: The timestamp/id of the note"
  :type '(alist :value-type (group string)) :group 'mvtn)

(defvar mvtn--link-regexp "\\^\\^[[:digit:]]\\{8\\}-[[:digit:]]\\{6\\}.*\\^\\^"
  "A regexp matching valid mvtn links.")

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


(defun mvtn-create-new-file (title tags)
  "Creates a new mvtn note file in
`mvtn-get-create-current-year-directory' with TITLE (string) as
title and TAGS (strings) as tags. The naming scheme follows the
following template:
\"{(`mvtn-current-timestamp' 'second)} {title} -- {tags}.{`mvtn-default-file-extension'}\"

Returns an open buffer with the new file.

Example: 
(mvtn-create-new-file \"Branching in Subversion\" '(\"dev\" \"subversion\"))
-> #<buffer 20210110-000548 Branching in Subversion -- dev subversion.org>"
  (let* ((tags-stripped (car (split-string tags)))
         (timestamp (mvtn-current-timestamp 'second))
         (file-name (format "%s/%s %s%s.%s"
                            (mvtn-get-create-current-year-directory)
                            timestamp title
                            (if tags-stripped (concat " -- " tags) "")
                            mvtn-default-file-extension))
         (template (mvtn-substitute-template
                    (mvtn-template-for-extension mvtn-default-file-extension)
                    title (format-time-string "%Y-%m-%d") timestamp))
         (buf (find-file-noselect file-name)))
    (with-current-buffer buf (insert template) (save-buffer)) buf))


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
-> \"/path/to/notes/2020/20210110-000548 Branching in Subversion.org\""
  (when (not (string-match-p mvtn--link-regexp link)) (error "Invalid mvtn link"))
  (let* ((timestamp (substring link 2 17))
         (year-dir (format "%s/%s/" mvtn-note-directory
                           (mvtn-timestamp-field timestamp 'year))))
    (mapcar (lambda (el) (concat year-dir el))
            (seq-filter (lambda (filename) (string-prefix-p timestamp filename))
                        (directory-files year-dir)))))


(defun mvtn-follow-link (link)
  "Follows the mvtn link LINK. If multiple matches exists,
prompts for disambiguation."
  (let* ((matches (mvtn-link-targets link))
         (target (cond ((> (length matches) 1)
                        (completing-read "Pick match: " matches))
                       ((= (length matches) 1)
                        (car matches))
                       (t (error "No matches found for link")))))
    (find-file target)))


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
(defun mvtn-jump-current-year-directory ()
  "Jump to {`mvtn-note-directory'}/{current-year}. Uses `dired'
or whatever `find-file' is configured to do for directories."
  (interactive)
  (find-file (mvtn-get-create-current-year-directory)))


;;;###autoload
(defun mvtn-new-note ()
  "Creates a new note using `mvtn-create-new-file'. Switches to
the buffer of the new note."
  (interactive)
  (let ((title (read-from-minibuffer "Title: "))
        (tags (read-from-minibuffer "Tags: ")))
    (switch-to-buffer (mvtn-create-new-file title tags))))


(provide 'mvtn)

;;; mvtn.el ends here
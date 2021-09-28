;;; mvtn-tag-addons.el --- Various helpers for tag interaction -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to provide some
;; convenience functions for interacting with tags.

;;; Code:

(require 'mvtn)
(require 'seq)
(require 'cl-lib)

;; ----------------------------------------------------------------------
;; Helpers
;; ----------------------------------------------------------------------

(defun mvtn-list-files-with-tags (tags &optional all)
  "Like `mvtn-list-files' except filter out all notes which do
not contain any of the specified TAGS."
  (let ((files (mvtn-list-files all)))
    (dolist (tag (split-string tags))
      (setq files (seq-filter
                   (lambda (el) (string-match-p (concat "--.+" tag) el)) files)))
    files))

;; ----------------------------------------------------------------------
;; Controlled Vocabulary
;; ----------------------------------------------------------------------

(defcustom mvtn-cv-file
  (concat (plist-get (car mvtn-note-directories) :dir) "/tags.txt")
  "A file where you can store a 'controlled vocabulary'.

To use this, first set `mvtn-cv-enable' to t.

Using a controlled vocabulary refers to the practice of only
using a predetermined set of tags.  Without a controlled
vocabulary, one might for example accidentaly use the tags 'dev'
and 'programming' or 'stud' and 'studying' interchangeably,
making tag searches much less useful.

The file format is best illustrated with an example:

stud :: Notes relating to university
lit  :: literature i am reading / have read
dev  :: programming/sysadmin/devops stuff

This example defines (only) three available tags: 'stud', 'lit'
and 'dev'.  Every line starts with a tag and then provides a short
description after two colons (:)."
  :type 'string :group 'mvtn)

(defun mvtn--get-string-from-file (file)
  "Return the content of FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun mvtn-cv-read-tags-from-file ()
  "Return a list of tags specified in `mvtn-cv-file'."
  (mapcar (lambda (el) (save-match-data
                    (string-match "\\([^ ]+\\) +:: \\(.+\\)" el)
                    (match-string-no-properties 1 el)))
          (split-string (mvtn--get-string-from-file mvtn-cv-file) "\n")))

;;;###autoload
(defun mvtn-cv-prompt-for-tags ()
  "Prompts for a selection of `mvtn-cv-read-tags-from-file'.
When a tag is not in the controlled vocabulary, the user is asked
wether they want to continue with the potentially incorrect tags
or try entering their tags again."
  (when (not (file-exists-p mvtn-cv-file))
    (error "%s does not exist.  Please create it" mvtn-cv-file))
  (let* ((cv (mvtn-cv-read-tags-from-file))
         (answer (completing-read-multiple "Tags (comma-separated): " cv))
         (continue t))
    (dolist (el answer)
      (if (and (not (member el cv))
               (not (y-or-n-p (concat "'" el "' is not in your controlled vocabulary. "
                                      "Continue anyway?"))))
          (setq continue nil)))
    (if continue answer (mvtn-cv-prompt-for-tags))))

;; ----------------------------------------------------------------------
;; `mvtn-tag-file-list'
;; ----------------------------------------------------------------------

;;;###autoload
(defun mvtn-tag-file-list (tags &optional all)
  "Display a list of all files matching all specified TAGS. With
a prefix argument, ignore `mvtn-search-years'."
  (interactive "MTags: \nP")
  (let ((buffer (get-buffer-create
                 (concat "*Mvtn Tag File List <" tags ">"
                         (if all " ALL" "") "*"))))
    (with-current-buffer buffer
      (mvtn-tag-file-list-mode)
      (mvtn--tag-file-list-refresh)
      (tabulated-list-print))
    (display-buffer buffer))
  nil)

(define-derived-mode mvtn-tag-file-list-mode tabulated-list-mode
  "Mvtn Tag File List"
  "The major-mode for `mvtn-tag-file-list'. Should likely not be
called on its own."
  (setq tabulated-list-format [("Title" 78 t) ("ID" 15 t)])
  (setq tabulated-list-sort-key (cons "ID" nil))
  (add-hook 'tabulated-list-revert-hook 'mvtn--tag-file-list-refresh nil t))

(define-key mvtn-tag-file-list-mode-map (kbd "o") 'mvtn-tag-file-list-open)
(define-key mvtn-tag-file-list-mode-map (kbd "C-o")
  'mvtn-tag-file-list-open-keep-focus)

(defun mvtn--tag-file-list-refresh ()
  "Called when an `mvtn-tag-file-list-mode' buffer is reverted or
created."
  (let ((tags (save-match-data
                (substring (buffer-name)
                           (1+ (string-match "<" (buffer-name)))
                           (string-match ">" (buffer-name)))))
        (all (string-match-p "> ALL" (buffer-name))))
    (setq tabulated-list-entries nil)
    (dolist (f (mvtn-list-files-with-tags tags all))
      (let* ((id-pos (string-match-p mvtn--id-regexp f))
             (fname `(,(substring f (+ id-pos 16))
                      face link
                      help-echo ,(format-message "Open note `%s'" f)
                      follow-link t
                      filename ,f
                      action mvtn--tag-file-list-action))
             (id `(,(substring f id-pos (+ id-pos 15)) face default)))
        (push (list f (vector fname id))
              tabulated-list-entries)))
    (tabulated-list-init-header)))

(defun mvtn--tag-file-list-action (button)
  "The action taken when a note in an `mvtn-tag-file-list' buffer
is clicked."
  (find-file
   (concat (mvtn-expand-note-name (button-get button 'filename)))))

(defun mvtn-tag-file-list-open (&optional keep-focus)
  "Open up the file under point in another window in a buffer
produced by `mvtn-tag-file-list'. If KEEP-FOCUS is non-nil, the
focus is kept in the `mvtn-tag-file-list-mode' buffer instead of
the opened file."
  (interactive "P")
  (let ((filename (mvtn-expand-note-name
                   (plist-get (cdr (elt (tabulated-list-get-entry (point)) 0))
                              'filename)))
        (prev-buffer (current-buffer)))
    (find-file-other-window filename)
    (if keep-focus (select-window (get-buffer-window prev-buffer)))))

(defun mvtn-tag-file-list-open-keep-focus ()
  "Call `mvtn-tag-file-list-open' with KEEP-FOCUS set to t."
  (interactive)
  (mvtn-tag-file-list-open t))

;; ----------------------------------------------------------------------
;; `mvtn-org-agenda'
;; ----------------------------------------------------------------------

(defvar org-agenda-files)

(defvar mvtn-org-agenda-tag "project"
  "All files with this tag will be added to `org-agenda-files'
when `mvtn-org-agenda-files-populate' is called.")

(defvar mvtn-org-agenda--initial-files nil
  "Stores the initial value of `org-agenda-files' before it is
modified through `mvtn-org-agenda-files-populate' to include all
notes tagged with `mvtn-org-agenda-tag'.")

(defun mvtn-org-agenda-files-populate ()
  "Adds all mvtn notes tagged with `mvtn-org-agenda-tag' to
`org-agenda-files'."
  (interactive)
  (when (not mvtn-org-agenda--initial-files)
    (setq mvtn-org-agenda--initial-files org-agenda-files))
  (setq org-agenda-files mvtn-org-agenda--initial-files)
  (dolist (file (mapcar 'mvtn-expand-note-name
                        (mvtn-list-files-with-tags mvtn-org-agenda-tag)))
    (when (not (member file org-agenda-files))
      (push file org-agenda-files))))

;;;###autoload
(defun mvtn-org-agenda (&optional arg org-keys restriction)
  "Like `org-agenda', except it calls
`mvtn-org-agenda-files-populate' before, thereby adding all notes
tagged with `mvtn-org-agenda-tag' to `org-agenda-files'."
  (interactive)
  (mvtn-org-agenda-files-populate)
  (funcall-interactively 'org-agenda arg org-keys restriction))

(provide 'mvtn-tag-addons)

;;; mvtn-tag-addons.el ends here
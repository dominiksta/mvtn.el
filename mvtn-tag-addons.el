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
  "List all notes with tags matching TAGS.
Ignore `mvtn-search-years' if ALL is non-nil."
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
                         (string-match "\\([^ ]+\\)[[:blank:]]*::[[:blank:]]*\\(.*\\)" el)
                         (match-string-no-properties 1 el)))
          (split-string (mvtn--get-string-from-file mvtn-cv-file) "\n")))

(defun mvtn-cv-write-tag-to-file (tag &optional description)
  "Write a new TAG into `mvtn-cv-file' with DESCRIPTION."
  (let* ((contents (mvtn--get-string-from-file mvtn-cv-file))
         (newline-at-end (or (string= contents "")
                             (string= (substring contents -1) "\n"))))
    (write-region (concat (if newline-at-end nil "\n") tag " :: " description)
                  nil mvtn-cv-file t)))

(defun mvtn--cv-multiaction-tag-prompt (tag)
  "Prompts for action for a TAG which is not specified in `mvtn-cv-file'."
  (if (and (>= emacs-major-version 26)
           (>= emacs-minor-version 2))
      (read-answer
       (concat "'" tag "' is not in your controlled vocabulary. "
               "What would you like to do? ")
       '(("continue" ?c "continue w/o adding tag to existing vocabulary file")
         ("add" ?a "continue and add tag to existing vocabulary file")
         ("edit" ?e "edit the tag list")
         ("remove"  ?r "remove this tag")
         ("continue all" ?C "perform yes action for all further unknown tags")
         ("add all" ?A "perform add action for all further unknown tags")
         ("help" ?h "show help")))
    ;; Compatibility for Emacs 26.1
    (nth 1 (read-multiple-choice
            (concat "'" tag "' is not in your controlled vocabulary. "
                    "What would you like to do? ")
            '((?c "continue" "continue w/o adding tag to existing vocabulary file")
              (?a "add" "continue and add tag to existing vocabulary file")
              (?e "edit" "edit the tag list")
              (?r "remove" "remove this tag")
              (?C "continue all" "perform yes action for all further unknown tags")
              (?A "add all" "perform add action for all further unknown tags"))))))

;;;###autoload
(defun mvtn-cv-prompt-for-tags (initial)
  "Prompts for a selection of `mvtn-cv-read-tags-from-file'.
When a tag is not in the controlled vocabulary, the user is asked
wether they want to continue with the potentially incorrect tags
or try entering their tags again.  INITIAL will already be
inserted in the minibuffer."
  ;; Allow for arbitrary return
  (catch 'ret
    (when (not (file-exists-p mvtn-cv-file))
      (mkdir (file-name-directory mvtn-cv-file) t)
      (write-region "" nil mvtn-cv-file t))
    (let* ((cv (mvtn-cv-read-tags-from-file))
           (taglist (completing-read-multiple "Tags (comma-separated): " cv
                                              nil nil initial))
           (continue-all nil)
           (add-all nil)
           (prompt-desc (lambda (tag) (read-from-minibuffer
                                  (format "Description for new tag (%s): "
                                          tag)))))
      (dolist (tag taglist)
        (unless (member tag cv)
          (let* ((read-answer-short t)
                 (user-answer (cond (continue-all "continue")
                                    (add-all "add")
                                    (t (mvtn--cv-multiaction-tag-prompt tag)))))
            ;; Continue w/o adding this supplied tags
            (cond ((string= user-answer "continue"))
                  ;; Add this supplied tag to cv
                  ((string= user-answer "add")
                   (mvtn-cv-write-tag-to-file tag (funcall prompt-desc tag)))
                  ;; Edit supplied tag list
                  ((string= user-answer "edit")
                   (setq taglist (mvtn-cv-prompt-for-tags
                                  (mapconcat 'identity taglist ",")))
                   ;; Exit recursion
                   (throw 'ret taglist))
                  ;; Remove from supplied tag list
                  ((string= user-answer "remove")
                   (setq taglist (delete tag taglist)))
                  ;; Continue w/o adding any supplied tag to cv
                  ((string= user-answer "continue all")
                   (setq continue-all t))
                  ;; Add all supplied tags to cv
                  ((string= user-answer "add all")
                   (mvtn-cv-write-tag-to-file tag (funcall prompt-desc tag))
                   (setq add-all t))))))
      taglist)))

;; ----------------------------------------------------------------------
;; `mvtn-tag-file-list'
;; ----------------------------------------------------------------------

;;;###autoload
(defun mvtn-tag-file-list (tags &optional all)
  "Display a list of all files matching all specified TAGS.
With a prefix argument (or setting ALL to non-nil), ignore
`mvtn-search-years'."
  (interactive
   (list (mapconcat 'identity
                    (if mvtn-cv-enable
                        (mvtn-cv-prompt-for-tags "") (mvtn-prompt-for-tags ""))
                    " ")
         current-prefix-arg))
  (let ((buffer (get-buffer-create
                 (concat "*Mvtn Tag File List <" tags ">"
                         (if all " ALL" "") "*"))))
    (with-current-buffer buffer
      (mvtn-tag-file-list-mode)
      (mvtn--tag-file-list-refresh)
      (setq-local tabulated-list-sort-key '("ID" . t))
      (tabulated-list-print))
    (display-buffer buffer))
  nil)

(define-derived-mode mvtn-tag-file-list-mode tabulated-list-mode
  "Mvtn Tag File List"
  "The major-mode for `mvtn-tag-file-list'."
  (setq tabulated-list-format [("Title" 78 t) ("ID" 15 t)])
  (setq tabulated-list-sort-key (cons "ID" nil))
  (add-hook 'tabulated-list-revert-hook 'mvtn--tag-file-list-refresh nil t))

(define-key mvtn-tag-file-list-mode-map (kbd "o") 'mvtn-tag-file-list-open)
(define-key mvtn-tag-file-list-mode-map (kbd "C-o")
  'mvtn-tag-file-list-open-keep-focus)

(defun mvtn--tag-file-list-refresh ()
  "Refresh the contents of an an `mvtn-tag-file-list-mode' buffer."
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
  "The action taken when clicking a note in `mvtn-tag-file-list-mode'.
BUTTON will refer to the button clicked."
  (find-file
   (concat (mvtn-expand-note-name (button-get button 'filename)))))

(defun mvtn-tag-file-list-open (&optional keep-focus)
  "Open the note under point in another window in `mvtn-tag-file-list-mode'.
If KEEP-FOCUS is non-nil, the focus is kept in the
`mvtn-tag-file-list-mode' buffer instead of the opened file."
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
  "See `mvtn-org-agenda-files-populate'.
All files with this tag will be added to `org-agenda-files' when
`mvtn-org-agenda-files-populate' is called.")

(defvar mvtn-org-agenda--initial-files nil
  "Stores the initial value of `org-agenda-files'.
The initial value is considered the value before its first
modification through `mvtn-org-agenda-files-populate' to include
all notes tagged with `mvtn-org-agenda-tag'.")

(defun mvtn-org-agenda-files-populate ()
  "Add all notes tagged with `mvtn-org-agenda-tag' to `org-agenda-files'."
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
  "A 'replacement' for `org-agenda'.
It just calls `org-agenda' after calling
`mvtn-org-agenda-files-populate', thereby adding all notes tagged
with `mvtn-org-agenda-tag' to `org-agenda-files'.
ARG, ORG-KEYS and RESTRICTION are passed to `org-agenda'."
  (interactive)
  (mvtn-org-agenda-files-populate)
  (funcall-interactively 'org-agenda arg org-keys restriction))

(provide 'mvtn-tag-addons)

;;; mvtn-tag-addons.el ends here
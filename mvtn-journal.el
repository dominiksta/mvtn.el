;;; mvtn-journal.el --- Simple journaling for mvtn -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to provide
;; journaling functionality akin to `org-journal'.  The reason `org-journal' is
;; not used directly is simply that it is hardcoded to use `org-mode', while
;; mvtn wants to also support `markdown-mode'.

;;; Code:

(require 'mvtn)

(defconst mvtn-journal-daily-timestamp "000000"
  "All daily notes will be created with this timestamp.
This is a constant and should not be changed.  Mvtn uses this
internally to identify daily notes.  Of course, this means that
you cannot create a note at /exactly/ midnight, as that would
confuse the journal.  This is reasonably unlikely to happen imo,
so this should never be a problem.")

(defcustom mvtn-journal-dir "prv/fleeting"
  "The abbreviated note directory to store journal files.
See `mvtn-note-directories' and `mvtn-short-note-dir-list'."
  :type 'string :group 'mvtn)

(defcustom mvtn-journal-new-daily-title "Daily note for %Y-%m-%d"
  "The title for all new daily notes.
Format specifiers from `format-time-string' can be used."
  :type 'string :group 'mvtn)

(defcustom mvtn-journal-new-daily-tags '("journal")
  "The tags for all new daily notes."
  :type '(list :value-type string) :group 'mvtn)

(defcustom mvtn-journal-file-extension-templates
  (list
   '("org" "#+TITLE: {title}
#+DATE: {date}
# mvtn_original_title :: {title}
# mvtn_original_id :: {timestamp}
#+STARTUP: fold\n\n")
   (assoc "md" mvtn-file-extension-templates)
   (assoc "" mvtn-file-extension-templates))
  "Like `mvtn-file-extension-templates', expept for journal notes."
  :type '(list :value-type string) :group 'mvtn)

(defcustom mvtn-journal-default-file-extension mvtn-default-file-extension
  "The default file extension for new journal notes.
Also controls which template from
`mvtn-journal-file-extension-templates' is used."
  :type 'string :group 'mvtn)

(defcustom mvtn-journal-entry-file-extension-templates
  '(("org" "* /%H:%M/ ") ("md" "# _%H:%M_ ") ("" "# %H:%M "))
  "The tags for all new daily notes."
  :type '(list :value-type string) :group 'mvtn)

(defun mvtn-journal-open-daily-for-time (time &optional encrypt)
  "Open the daily note for TIME, creating it if necessary.
TIME is defined in the emacs-internal time format.  See
`current-time' for the format.  A simple function to create such
a timestamp is `encode-time'.  When the note does not yet exist
and ENCRYPT is non-nil, the new note will be encrypted with gpg."
  (let* ((id (format-time-string
              (concat "%Y%m%d-" mvtn-journal-daily-timestamp) time))
         (link (format "^^%s^^" id))
         (title (format-time-string mvtn-journal-new-daily-title time))
         (date (format-time-string "%Y-%m-%d" time))
         (found (mvtn-link-targets link)))
    (when (> (length found) 1)
      (error "Multiple daily entries for one day: %s" found))
    (if found
        (progn
          (when encrypt (warn (concat "The current daily note has already "
                                      "been created as an unencrypted file")))
          (mvtn-follow-link link))
      (progn
        (message "Creating new daily note for %s" date)
        (switch-to-buffer
         (mvtn-create-new-file
          id mvtn-journal-dir title
          mvtn-journal-default-file-extension mvtn-journal-new-daily-tags
          (mvtn-substitute-template
           (mvtn-template-for-extension mvtn-journal-default-file-extension
                                        mvtn-journal-file-extension-templates)
           title date id)
          encrypt))))))

(defun mvtn-journal-open-daily-past-relative (n &optional encrypt)
  "Open the daily note for N days in the past.
If ENCRYPT is specified and the note does not yet exist, if will
be encrypted with gpg."
  (interactive)
  (let* ((target-time (seconds-to-time (- (time-to-seconds)
                                          (* 60 (* 60 (* 24 n)))))))
    (mvtn-journal-open-daily-for-time target-time encrypt)))

(defun mvtn-journal-new-entry-for-time (time text &optional encrypt)
  "Insert TEXT at a new entry at TIME in the daily note for TIME.
If ENCRYPT is specified and the daily note for TIME does not yet
exist, if will be encrypted with gpg."
  (mvtn-journal-open-daily-for-time time encrypt)
  (goto-char (point-max))
  (insert "\n"
          (format-time-string (mvtn-template-for-extension
                               mvtn-journal-default-file-extension
                               mvtn-journal-entry-file-extension-templates)
                              time)
          text)
  (when (fboundp 'evil-append) (evil-append 0)))

;;;###autoload
(defun mvtn-journal-new-entry (&optional encrypt)
  "Open the journal for today and add a new entry for the current time.
If ENCRYPT is specified and the daily note for TIME does not yet
exist, if will be encrypted with gpg."
  (interactive "P")
  (mvtn-journal-new-entry-for-time (current-time) "" encrypt))

;;;###autoload
(defun mvtn-journal-new-quick-entry (text &optional encrypt)
  "Create a new journal entry for the current time from TEXT.
In interactive use, TEXT will be read from the minibuffer.  Like
`mvtn-journal-new-entry', but does not open the file in the
foreground.  If ENCRYPT is specified and the daily note for TIME
does not yet exist, if will be encrypted with gpg."
  (interactive "MJournal Entry: \nP")
  (save-window-excursion
    (mvtn-journal-new-entry-for-time (current-time) text encrypt)
    (when (fboundp 'evil-normal-state) (evil-normal-state))
    (save-buffer)))

;;;###autoload
(defun mvtn-journal-new-entry-yesterday (&optional encrypt)
  "Open yesterdays daily note.
If ENCRYPT is specified and the note does not yet exist, if will
be encrypted with gpg."
  (interactive)
  (mvtn-journal-open-daily-past-relative 1 encrypt))

(provide 'mvtn-journal)

;;; mvtn-journal.el ends here
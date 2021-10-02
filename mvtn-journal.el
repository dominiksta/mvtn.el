;;; mvtn-journal.el --- Simple journaling for mvtn -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to provide
;; journaling functionality akin to `org-journal'.  The reason `org-journal' is
;; not used directly is simply that it is hardcoded to use `org-mode', while
;; mvtn wants to also support `markdown-mode'.

;;; Code:

(require 'mvtn)
(require 'mvtn-file-helpers)

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

;; ----------------------------------------------------------------------
;; autojournal
;; ----------------------------------------------------------------------

;;;###autoload
(defun mvtn-journal-autojournal-set-feature (feature enable)
  "Enable or disable an mvtn autojournaling feature.
Autojournaling refers to the concept of automatically adding
entries to your journal on certain events.  For example, if you
use `magit', you can put

\(mvtn-journal-autojournal-set-feature 'git-commit t)

in your init file to enable automatically logging all git commits
to your journal.

If ENABLE is non-nil, the feature will be enabled, otherwise
disabled.

Available values for FEATURE are:
- 'git-commit (see `mvtn-journal-autojournal-git-commit')
- 'note-changed (see `mvtn-journal-autojournal-note-changed')
- 'org-clock (see `mvtn-journal-autojournal-org-clock')"
  (let* ((suffix (if enable "-enable" "-disable")))
    (funcall (intern (concat "mvtn-journal-autojournal-"
                             (symbol-name feature) suffix)))))

;; git-commit
;; ----------------------------------------------------------------------

(defcustom mvtn-journal-autojournal-git-commit-format
  "Commit in repo \"%s\"\n\n- Commit Summary: %s\n- Full repo path: %s\n"
  "The format for autojournal entries for git commits.
The first %s will be replaced by the repository, the second by
the commit summary and the third with the full path to the
repository.  See `mvtn-journal-autojournal-set-feature' and
`mvtn-journal-autojournal-git-commit'."
  :type 'string :group 'mvtn)

(declare-function magit-toplevel "ext:magit")
(declare-function magit-git-string "ext:magit")
(declare-function magit-format-rev-summary "ext:magit")

(defun mvtn-journal-autojournal-git-commit ()
  "Record a commit made with magit to the mvtn journal.
Expects to be called using `git-commit-post-finish-hook'.
Catches errors and ignores them because the ability to commit
seems more important than journaling."
  (condition-case nil
      (let* ((repo (abbreviate-file-name (magit-toplevel)))
             (short-repo (file-name-nondirectory (directory-file-name repo)))
             (rev (magit-git-string "rev-parse" "HEAD"))
             (summary (substring-no-properties (magit-format-rev-summary rev))))
        (mvtn-journal-new-quick-entry
         (format mvtn-journal-autojournal-git-commit-format
                 short-repo summary repo)))
    (error nil)))

(defun mvtn-journal-autojournal-git-commit-enable ()
  "Enable mvtn autojournal for git commits."
  (add-hook 'git-commit-post-finish-hook 'mvtn-journal-autojournal-git-commit))
(defun mvtn-journal-autojournal-git-commit-disable ()
  "Disable mvtn autojournal for git commits."
  (remove-hook 'git-commit-post-finish-hook 'mvtn-journal-autojournal-git-commit))


;; note-changed
;; ----------------------------------------------------------------------

(defcustom mvtn-journal-autojournal-note-changed-format
  "Edited note: %s\n\nedit:%s %s\n"
  "The format for autojournal entries for note edits.
The first %s will be replaced by the note name, the second by a
the current timestamp and the third link to the note.  See
`mvtn-journal-autojournal-set-feature' and
`mvtn-journal-autojournal-note-changed'."
  :type 'string :group 'mvtn)

(defun mvtn-journal-autojournal-note-changed ()
  "Record every edit of any mvtn note to the mvtn journal.
Expects to be called using `after-save-hook'.  Catches errors and
ignores them because the ability to save a file seems more
important than journaling."
  (condition-case nil
      (let* ((id+name (mvtn--extract-note-identity buffer-file-name t))
             (link (format "^^%s^^" id+name))
             (name (save-match-data
                      (string-match mvtn--named-link-regexp link)
                      (match-string-no-properties 4 link)))
             (today (format-time-string "%Y%m%d"))
             (match (format "^%s-%s" today mvtn-journal-daily-timestamp)))
        ;; ignore the current journal note to avoid an infinite loop
        (if (not (string-match-p match id+name))
            (mvtn-journal-new-quick-entry
             (format mvtn-journal-autojournal-note-changed-format
                     name (format-time-string "%H:%M:%S") link))))
    (error nil)))

(defun mvtn-journal-autojournal-note-changed-enable ()
  "Enable mvtn autojournal for mvtn note edits."
  (add-hook 'mvtn-minor-mode-hook
            (lambda () (add-hook 'before-save-hook
                            'mvtn-journal-autojournal-note-changed 0 t))))
(defun mvtn-journal-autojournal-note-changed-disable ()
  "Disable mvtn autojournal for mvtn note edits."
  (remove-hook 'mvtn-minor-mode-hook
               (lambda () (add-hook 'before-save-hook
                               'mvtn-journal-autojournal-note-changed 0 t))))

;; org-clock
;; ----------------------------------------------------------------------

(defcustom mvtn-journal-autojournal-org-clock-in-note-format
  "org-clock\n\nclock-in:%s: %s in %s\n"
  "The format for autojournal entries for org clock in events in mvtn notes.
The first %s will be replaced by current timestamp, the second by
the org task and the third by a link to the note.  See
`mvtn-journal-autojournal-set-feature' and
`mvtn-journal-autojournal-note-changed'."
  :type 'string :group 'mvtn)

(defcustom mvtn-journal-autojournal-org-clock-out-note-format
  "org-clock\n\nclock-out:%s: %s in %s\n"
  "The format for autojournal entries for org clock out events in mvtn notes.
The first %s will be replaced by current timestamp, the second by
the org task and the third by a link to the note.  See
`mvtn-journal-autojournal-set-feature' and
`mvtn-journal-autojournal-note-changed'."
  :type 'string :group 'mvtn)

(defcustom mvtn-journal-autojournal-org-clock-in-file-format
  "org-clock\n\nclock-in:%s: %s in %s\n"
  "The format for autojournal entries for org clock in events regular files.
The first %s will be replaced by current timestamp, the second by
the org task and the third by the full file path.  See
`mvtn-journal-autojournal-set-feature' and
`mvtn-journal-autojournal-note-changed'."
  :type 'string :group 'mvtn)

(defcustom mvtn-journal-autojournal-org-clock-out-file-format
  "org-clock\n\nclock-out:%s: %s in %s\n"
  "The format for autojournal entries for org clock out events regular files.
The first %s will be replaced by current timestamp, the second by
the org task and the third by the full file path.  See
`mvtn-journal-autojournal-set-feature' and
`mvtn-journal-autojournal-note-changed'."
  :type 'string :group 'mvtn)

(defvar org-clock-current-task)

(defun mvtn-journal-autojournal-org-clock (in)
  "Record an org clock in or out event to the mvtn journal.
If IN is non-nil, a clock-in event is logged.  Is IN is nil, a
clock-out event is logged.  Catches errors and ignores them
because the ability to clock in or out seems more important than
journaling that event."
  (condition-case nil
      (let* ((task-name (substring-no-properties org-clock-current-task))
             (file (abbreviate-file-name buffer-file-name))
             (file-short (file-name-nondirectory buffer-file-name))
             (is-note (string-match-p mvtn--link-regexp (format "^^%s^^" file-short)))
             (id+name (if is-note (mvtn--extract-note-identity
                                   buffer-file-name t)
                        nil))
             (link (if is-note (format "^^%s^^" id+name)))
             (time (format-time-string "%H:%M:%S"))
             (text (if in
                       (if is-note
                           (format mvtn-journal-autojournal-org-clock-in-note-format
                                   time task-name link)
                         (format mvtn-journal-autojournal-org-clock-in-file-format
                                 time task-name file))
                     (if is-note
                         (format mvtn-journal-autojournal-org-clock-out-note-format
                                 time task-name link)
                       (format mvtn-journal-autojournal-org-clock-out-file-format
                               time task-name file)))))
        (mvtn-journal-new-quick-entry text))
    (error nil)))

(defun mvtn-journal-autojournal-org-clock-in ()
  "Call (`mvtn-journal-autojournal-org-clock' t)."
  (mvtn-journal-autojournal-org-clock t))
(defun mvtn-journal-autojournal-org-clock-out ()
  "Call (`mvtn-journal-autojournal-org-clock' nil)."
  (mvtn-journal-autojournal-org-clock nil))

(defun mvtn-journal-autojournal-org-clock-enable ()
  "Enable mvtn autojournal for org clock."
  (add-hook 'org-clock-in-hook 'mvtn-journal-autojournal-org-clock-in)
  (add-hook 'org-clock-out-hook 'mvtn-journal-autojournal-org-clock-out))
(defun mvtn-journal-autojournal-org-clock-disable ()
  "Disable mvtn autojournal for org clock."
  (remove-hook 'org-clock-in-hook 'mvtn-journal-autojournal-org-clock-in)
  (remove-hook 'org-clock-out-hook 'mvtn-journal-autojournal-org-clock-out))

(provide 'mvtn-journal)

;;; mvtn-journal.el ends here
;;; mvtn-backlink-buffer.el --- Display backlinks in a side buffer -*- lexical-binding: t -*-

;;; Commentary:

;; Provides the function `mvtn-backlink-buffer-toggle-side-window' which toggles
;; the visibility of a buffer containing backlinks to the currently open mvtn
;; note.

;;; Code:

(require 'mvtn)
(require 'mvtn-link-buttons)
(require 'org)


(defcustom mvtn-search-async-command
  (if (eq mvtn-search-function 'mvtn-search-full-text-rg) 'rg 'grep)
  "The command-line program used to populate the backlink buffer.
Can be one of '(grep rg)."
  :type 'symbol :group 'mvtn)

(defcustom mvtn-backlink-buffer-heading-face '(:inherit font-lock-string-face)
  "How headings should be displayed in the backlink buffer.
Headings refer to the actual backlinks here."
  :type 'list :group 'mvtn)

(defcustom mvtn-backlink-buffer-side-window-side 'right
  "On what side to open the backlink buffer.
Can be one of '(top bottom left right)."
  :type 'symbol :group 'mvtn)

(defcustom mvtn-backlink-buffer-mode-line-format " *mvtn-backlinks*"
  "The format of the backlink buffers mode-line.
See `mode-line-format' for further details."
  :type 'string :group 'mvtn)

(defvar mvtn-backlink-buffer--log-updates nil
  "Log updates of the backlink buffer to the *Messages* buffer.")

(defvar mvtn-backlink-buffer--title ""
  "The title of the current backlink buffer.")

(defconst mvtn-search-async-buffer "*mvtn-search-async-output*"
  "The name of the process buffer populating the backlink buffer.")

(defconst mvtn-backlink-buffer "*mvtn-backlinks*"
  "The name of the backlink buffer.")

(defun mvtn-backlink-buffer-parse-command-output (command-output)
  "Parse a COMMAND-OUTPUT from `mvtn-search-full-text-grep-async-command'.
The result is a list of property lists, each with the keys :file,
:line and :text."
  (save-match-data
    (let ((text-tmp "") (result nil))
      (dolist (line (butlast (split-string command-output "\n")))
        (cond
         ;; lines with context ("-B" flag)
         ((string-match "\\(.+\\) \\([0-9]+\\)-\\(.+\\)" line)
          (setq text-tmp (concat text-tmp
                                 (match-string-no-properties 3 line) "\n  ")))
         ;; lines matching search query
         ((string-match "\\(.+\\) \\([0-9]+\\):\\(.+\\)" line)
          (push (list :file (match-string-no-properties 1 line)
                      :line (match-string-no-properties 2 line)
                      :text (concat text-tmp
                                    (match-string-no-properties 3 line)))
                result)
          (setq text-tmp ""))))
      result)))

(defun mvtn-backlink-buffer-sentinel (process signal)
  "Runs after the command from `mvtn-backlink-buffer-populate' has exited.
When the command was successful, it will populate the backlink
buffer.  PROCESS will be the process created in
`mvtn-backlink-buffer-populate' and SIGNAL the signal the process
is emitting."
  (ignore signal)
  ;; (print (format "signal: %s" (process-exit-status process)))
  ;; (print (format "status: %s" (process-status process)))
  (when mvtn-backlink-buffer--log-updates
    (message "Updating the backlink buffer"))
  (when (memq (process-status process) '(exit))
    (let ((code (process-exit-status process)))
      (when (not (or (eq code 0) (eq code 1)))
        (error "Process failed with unexpected exit-code %s" code)))
    (let* ((result (with-current-buffer (process-buffer process)
                     (buffer-substring-no-properties (point-min) (point-max))))
           (parsed (mvtn-backlink-buffer-parse-command-output result)))
      (kill-buffer (process-buffer process))
      (with-current-buffer (get-buffer-create mvtn-backlink-buffer)
        (org-mode)
        (setq-local org-hide-emphasis-markers t)
        (visual-line-mode 1)
        (setq-local mode-line-format mvtn-backlink-buffer-mode-line-format)
        (apply 'face-remap-add-relative
               (append '(button) mvtn-backlink-buffer-heading-face))
        (mvtn-link-buttons-fontify)
        (read-only-mode 0)
        (erase-buffer)
        (insert (format "*%s*\n" (substring mvtn-backlink-buffer--title 16)))
        (insert "==============================\n\n")
        (cond ((= (length parsed) 0) (insert "No Backlinks found."))
              ((= (length parsed) 1) (insert (format "%d Backlink:\n\n"
                                                     (length parsed))))
              ((> (length parsed) 1) (insert (format "%d Backlinks:\n\n"
                                                     (length parsed)))))
        (dolist (el parsed)
          (let* ((id+name (mvtn--extract-note-identity (plist-get el :file) t))
                 (name (substring id+name 16))
                 (id (substring id+name 0 15))
                 (link (format "^^%s :l: %s^^" id+name (plist-get el :line))))
            (insert "- ")
            (insert-button (format "%s" name)
                           'button t
                           'help-echo (format "Follow link: %s" link)
                           'mvtn-link link
                           'action (lambda (b) (mvtn-follow-link (button-get b 'mvtn-link)))
                           'keymap '(keymap
                                     (mouse-1 . push-button)
                                     (13 . push-button)) ;; RET
                           ))
          (insert (format "\n  %s\n\n" (plist-get el :text))))
        (read-only-mode 1)))))

(defun mvtn-search-full-text-grep-async-command (string)
  "Return a grep command for a full text search in all notes.
Searches for the regexp STRING."
  (let ((default-directory (plist-get (car mvtn-note-directories) :dir)))
    (append (list mvtn-grep-program "-nHrIZ" "--before-context=2" string)
            (mvtn--search-dirs))))

(defun mvtn-backlink-buffer-populate ()
  "Start a process that will populate the backlink buffer."
  (when (not (member mvtn-search-async-command '(grep rg)))
    (error "Invalid value of `mvtn-search-async-command': %s"
           mvtn-search-async-command))
  (setq mvtn-backlink-buffer--title
        (mvtn--extract-note-identity (buffer-file-name) t))
  (let ((default-directory (plist-get (car mvtn-note-directories) :dir)))
    (make-process
     :name "mvnt-backlink-search"
     :buffer (generate-new-buffer mvtn-search-async-buffer)
     :command (funcall (cond ((eq mvtn-search-async-command 'grep)
                              'mvtn-search-full-text-grep-async-command)
                             ((eq mvtn-search-async-command 'rg)
                              'mvtn-search-full-text-rg-async-command))
                       (format "\\^\\^%s" (mvtn--extract-note-identity
                                           (buffer-file-name))))
     :noquery t
     :sentinel 'mvtn-backlink-buffer-sentinel)))

(define-minor-mode mvtn-backlink-buffer-side-window-mode
  "This minor mode will enable automatic updates of the backlink buffer.
Should not be used manually as calling
`mvtn-backlink-buffer-toggle-side-window' is more intuitive."
  nil " backlinks" nil
  :global t
  (if (bound-and-true-p mvtn-backlink-buffer-side-window-mode)
      (progn
        (add-hook 'window-state-change-hook
                  'mvtn-backlink-buffer-maybe-update-side-window))
    (progn
      (remove-hook 'window-state-change-hook
                   'mvtn-backlink-buffer-maybe-update-side-window))))

(defun mvtn-backlink-buffer-maybe-update-side-window ()
  "Maybe run `mvtn-backlink-buffer-populate'.
Only runs said function if in `mvtn-minor-mode' and the
`mvtn-backlink-buffer' is visible."
  (when (and (bound-and-true-p mvtn-minor-mode)
             (get-buffer-window mvtn-backlink-buffer))
    (mvtn-backlink-buffer-populate)))

;;;###autoload
(defun mvtn-backlink-buffer-toggle-side-window (action)
  "Toggle visibility of a side window showing backlinks to the current note.
When called interactively, the visibility is toggled.  When
called programmatically, the visibility is defined by
ACTION (either 'show or 'hide).  See
`mvtn-backlink-buffer-side-window-side' for customization.  Uses
`mvtn-search-async-command' to search for backlinks."
  (interactive '(nil))
  (let ((win (get-buffer-window mvtn-backlink-buffer)))
    (when (or (eq action 'show) (and (eq action nil) (not win)))
      (mvtn-backlink-buffer-side-window-mode 1)
      (display-buffer-in-side-window
       (get-buffer-create mvtn-backlink-buffer)
       (list (cons 'side mvtn-backlink-buffer-side-window-side))))
    (when (and win (or (eq action 'hide) (eq action nil)))
      (delete-window win))))

(provide 'mvtn-backlink-buffer)

;;; mvtn-backlink-buffer.el ends here

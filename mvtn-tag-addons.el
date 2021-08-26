;;; mvtn-tag-addons.el --- Various helpers for tag interaction -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to provide some
;; convenience functions for interacting with tags.

;;; Code:

(require 'mvtn)
(require 'seq)

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

(provide 'mvtn-tag-addons)
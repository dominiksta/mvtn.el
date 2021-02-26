;;; mvtn-file-helpers.el --- Various helpers for file interaction -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to provide some
;; convenience functions for interacting with files.

;;; Code:

(require 'mvtn)

(declare-function dired-get-filename "ext:dired")

;;;###autoload
(defun mvtn-rename-current-file (name)
  "Changes the title depending on the major-mode and renames the
file. In org-mode, it uses \"#+TITLE:\" to look for the title. In
markdown-mode, it looks for the first top-level headline (a line
starting with \"# \") and in any other mode it looks for the
first occurence of \"title: \"."
  (interactive "MNew filename: ")
  (let* ((old-orig (file-name-nondirectory (buffer-file-name (current-buffer))))
         (old-timestamp (substring old-orig 0 15))
         (old-ext (file-name-extension old-orig))
         (old-name+tags (substring old-orig 16 (- -1 (length old-ext))))
         (old-tags (cadr (split-string old-name+tags " -- ")))
         (new (format "%s %s%s.%s" old-timestamp name
                      (if old-tags (concat " -- " old-tags) "") old-ext)))
    (rename-file old-orig new)
    (rename-buffer new)
    (set-visited-file-name new))
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (cond ((eq major-mode 'org-mode) (re-search-forward "^#\\+TITLE: "))
            ((eq major-mode 'markdown-mode) (re-search-forward "^# "))
            (t (re-search-forward "^title: "))))
    (kill-line)
    (insert name)
    (save-buffer)))

;;;###autoload
(defun mvtn-import-file (filename title tags &optional move)
  "Import the file given by FILENAME into
`mvtn-get-create-current-year-directory' with the new filename
given by `mvtn-generate-file-name'. By default, the file is
copied. If MOVE is non-nil, the file is instead moved and the
buffer visiting the file is pointed to the new direction - if
such a buffer exists."
  (interactive (let* ((filename (read-file-name "Filename: "))
                      (title (read-string
                              "Title: " (file-name-sans-extension
                                         (file-name-nondirectory filename))))
                      (tags (read-string "Tags: ")))
                 (list filename title tags)))
  (let* ((ext (or (file-name-extension filename) "txt"))
         (mvtn-name (mvtn-generate-file-name
                     (mvtn-current-timestamp 'second) title ext tags)))
    (if move
        (progn (rename-file filename mvtn-name)
               (when (buffer-live-p (find-buffer-visiting filename))
                 (with-current-buffer (find-buffer-visiting filename)
                   (rename-buffer mvtn-name)
                   (set-visited-file-name mvtn-name))))
      (copy-file filename mvtn-name))
    (message "Imported %s into %s" filename mvtn-name)))

;;;###autoload
(defun mvtn-import-file-dired ()
  "Use `dired-get-filename' to populate the FILENAME argument of
`mvtn-import-file' (which see for further details)."
  (interactive)
  (require 'dired)
  (let* ((filename (dired-get-filename))
         (title (read-string "Title: " (file-name-sans-extension
                                        (file-name-nondirectory filename))))
         (tags (read-string "Tags: "))
         (move (not (yes-or-no-p "Keep original file?"))))
    (mvtn-import-file filename title tags move)))

(provide 'mvtn-file-helpers)

;;; mvtn-file-helpers.el ends here
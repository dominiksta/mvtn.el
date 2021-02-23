;;; mvtn-file-helpers.el --- Various helpers for file interaction -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to provide some
;; convenience functions for interacting with files.

;;; Code:

(require 'mvtn)

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

(provide 'mvtn-file-helpers)

;;; mvtn-file-helpers.el ends here
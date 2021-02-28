;;; mvtn-compat.el --- Support for emacs <27 for mvtn -*- lexical-binding: t -*-

(require 'seq)
(defvar mvtn-excluded-directories)

(defun mvtn-list-files-function-native (&optional search)
  "Native (elisp) implementation for `mvtn-list-files-function'.
Does not show hidden files (prefixed with '.').
Emacs <27 compatibility - less performant since it needs to traverse all
excluded directories and only filters afterwards."
  (mapcar (lambda (file-name)
            (substring file-name (1+ (length default-directory))))
          (seq-filter (lambda (file)
                        (not (member
                              nil (mapcar (lambda (dir) (not (string-match-p
                                                         (format "/%s/" dir) file)))
                                          mvtn-excluded-directories))))
                      (sort (directory-files-recursively
                             "." (if search
                                     (format "^[^\\.]*%s" search)
                                   "^[^\\.]")
                             nil)
                            'string<))))

(provide 'mvtn-compat)

;;; mvtn-compat.el ends here
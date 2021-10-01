;;; mvtn-test-setup.el --- Setup a note directory for testing -*- lexical-binding: t -*-

(require 'mvtn-test-helpers)

(defvar mvtn-test-note-dir (expand-file-name "test/test-notes")
  "A folder to create/rename/delete some actual note
files. Mocking seemed like too much of a hassle here.")

(defvar mvtn-test-note-dirs
  (list (list :dir (expand-file-name "test/test-notes/private") :name "prv" :structure
              '((:dir "zettelkasten" :datetree t)
                (:dir "devlog" :datetree t)
                (:dir "fleeting" :datetree t)
                (:dir "static" :datetree nil)))
        (list :dir (expand-file-name "test/test-notes/work") :name "wrk" :structure
              '((:dir "meetings" :datetree t)
                (:dir "static" :datetree nil))))
  "A folder to create/rename/delete some actual note
files. Mocking seemed like too much of a hassle here.")

(setq mvtn-note-directories mvtn-test-note-dirs)

(provide 'mvtn-test-setup)

;;; mvtn-test-setup.el ends here

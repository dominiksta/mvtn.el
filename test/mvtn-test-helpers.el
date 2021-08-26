;;; mvtn-test-helpers.el --- Helpers for mvtn unit test -*- lexical-binding: t -*-

(require 'seq)

(defun mvtn-test-file-exists-disregarding-timestamp-p (filename dir)
  "Check wether FILENAME exists in DIR, disregarding mvtn
timestamps. Therefore, TIMESTAMP has to be provided *without* the
timestamp. Example:

(mvtn-test-file-exists-disregarding-timestamp-p
  \"My filename -- tag1.org\" \"somedir\")"
  (seq-filter (lambda (el)
                (string-match-p (concat "^[[:digit:]]\\{8\\}-[[:digit:]]\\{6\\} "
                                        filename)
                                el))
              (directory-files dir)))


(defun mvtn-test-touch (filename)
  "Same as the unix command \"touch FILENAME\""
  (write-region "" nil filename))


(defmacro mvtn-test-with-testfiles (no-delete &rest body)
  `(let ((mvtn-note-directories mvtn-test-note-dirs))
     (when (file-exists-p mvtn-test-note-dir)
       (delete-directory mvtn-test-note-dir t))
     (mkdir mvtn-test-note-dir t) (cd mvtn-test-note-dir)
     (mkdir (plist-get (car mvtn-test-note-dirs) :dir) t)
     (cd (plist-get (car mvtn-test-note-dirs) :dir))
     (mkdir "zettelkasten") (cd "zettelkasten")
     (mkdir "1999") (cd "1999")
     (mvtn-test-touch "19990110-134522 test1 -- tags test.txt")
     (mvtn-test-touch "19990110-134522 test2 test2.txt")
     (write-region "title: test2 test2
date: 1999-01-10
mvtn_original_title :: test2 test2
mvtn_original_id :: 19990110-134522
----------------------------------------------------------------------\n\n"
                   nil  "19990110-134522 test2 test2.txt")
     (mvtn-test-touch "19990110-134523 test3 test3.org")
     (cd "..") (mkdir "2018") (cd "2018")
     (mvtn-test-touch "20181212-134541 test1.txt")
     (write-region "title: test1
date: 2018-12-12
mvtn_original_title :: test1 test1
mvtn_original_id :: 20181212-134541
----------------------------------------------------------------------

This is some content.
And some more content."
                   nil  "20181212-134541 test1.txt")
     (mvtn-test-touch "20181212-134542 test2 test2 -- tags tags tags.txt")
     (mvtn-test-touch "20181212-134544 test3 test3.org")
     (cd "..") (mkdir "2020") (cd "2020")
     (mvtn-test-touch "20201212-134541 test1.txt")
     (mvtn-test-touch "20201212-134541 test1 (fake conflicted copy).txt")
     (mvtn-test-touch "20201212-134542 test2 test2 -- tags tags tags.txt")
     (mvtn-test-touch "20201212-134544 test3 test3.org")
     (cd "..") (mkdir "2021") (cd "2021")
     (mvtn-touch-new-file "prv/zettelkasten" "20210110-134522" "test1" "md" "i have tags")
     (mvtn-touch-new-file "prv/zettelkasten" "20210110-134523" "test2 test2" "txt" "")
     (mvtn-test-touch "20210110-134524 test3 test3.org")
     (cd "../..") (mkdir "static") (cd "static")
     (mvtn-test-touch "20130210-134522 an old statically displayed note.md")
     (mvtn-test-touch "20130210-134522 an old statically displayed note.org")
     (mkdir "ltximg") (cd "ltximg")
     (mvtn-test-touch "someimage.png") (mvtn-test-touch "someimage2.png")
     (mvtn-test-touch "20130210-123456 a note in an excluded folder.md")
     (cd mvtn-test-note-dir)
     (mkdir (plist-get (cadr mvtn-test-note-dirs) :dir))
     (cd (plist-get (cadr mvtn-test-note-dirs) :dir))
     (mkdir "meetings") (cd "meetings")
     (mkdir "2019") (cd "2019")
     (mvtn-test-touch "20190210-134522 a note for work 1.md")
     (mvtn-test-touch "20190210-134522 a note for work 2.org")
     (let ((result (progn ,@body)))
       (when (not ,no-delete) (delete-directory mvtn-test-note-dir t))
       (cd (concat mvtn-test-note-dir "/.."))
       result)))

(provide 'mvtn-test-helpers)

;;; mvtn-test-helpers.el ends here
;;; mvtn-test-helpers.el --- summary -*- lexical-binding: t -*-

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


(defmacro mvtn-test-with-testfiles (&rest body)
  `(progn
     (delete-directory mvtn-test-note-dir t)
     (mkdir mvtn-test-note-dir) (cd mvtn-test-note-dir)
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
     (mvtn-test-touch "20181212-134542 test2 test2 -- tags tags tags.txt")
     (mvtn-test-touch "20181212-134544 test3 test3.org")
     (cd "..") (mkdir "2020") (cd "2020")
     (mvtn-test-touch "20201212-134541 test1.txt")
     (mvtn-test-touch "20201212-134541 test1 (fake conflicted copy).txt")
     (mvtn-test-touch "20201212-134542 test2 test2 -- tags tags tags.txt")
     (mvtn-test-touch "20201212-134544 test3 test3.org")
     (cd "..") (mkdir "2021") (cd "2021")
     (mvtn-test-touch "20210110-134522 test1 -- i have tags.md")
     (mvtn-test-touch "20210110-134523 test2 test2.txt")
     (mvtn-test-touch "20210110-134524 test3 test3.org")
     (cd "../..")
     (let* ((mvtn-note-directory mvtn-test-note-dir)
            (result (progn ,@body)))
       (delete-directory mvtn-test-note-dir t)
       result)))

(provide 'mvtn-test-helpers)

;;; mvtn-test-helpers.el ends here
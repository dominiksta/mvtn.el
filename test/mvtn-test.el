;;; mvtn-test.el --- Unit tests for mvtn -*- lexical-binding: t -*-

(require 'mvtn)
(require 'mvtn-test-helpers)
(require 'ert)
(require 'thingatpt)


;; NOTE: This is a personal project, not "enterprise" code. I did not invest
;; time in good coverage or anything of the sort. These are just basic tests to
;; (hopefully) catch basic bugs.


(defvar mvtn-test-note-dir (expand-file-name "test-notes")
  "A folder to create/rename/delete some actual note
files. Mocking seemed like too much of a hassle here.")

(defvar mvtn-test-note-dirs
  (list (list :dir (expand-file-name "test-notes/private") :name "prv" :structure
              '((:dir "zettelkasten" :datetree t)
                (:dir "devlog" :datetree t)
                (:dir "static" :datetree nil)))
        (list :dir (expand-file-name "test-notes/work") :name "wrk" :structure
              '((:dir "meetings" :datetree t)
                (:dir "static" :datetree nil))))
  "A folder to create/rename/delete some actual note
files. Mocking seemed like too much of a hassle here.")


(ert-deftest mvtn-test-link-regexp ()
  "Test `mvtn--link-regexp'"
  (should (string-match-p mvtn--link-regexp
                          "^^20210110-135222 testname -- testtag.org^^"))
  (should (string-match-p
           mvtn--link-regexp
           "^^20210110-135222 testname with space -- testtag.org^^"))
  (should (string-match-p mvtn--link-regexp
                          "^^20210110-135222 no tag.org^^"))
  (should (not (string-match-p mvtn--link-regexp
                               "^^20210110-13522 invalid timestamp.org^^")))
  (should (not (string-match-p mvtn--link-regexp
                               "^^202101a0-135222 invalid timestamp.org^^"))))


(ert-deftest mvtn-test-timestamp-field ()
  "Test `mvtn-timestamp-field'"
  (should (string-equal (mvtn-timestamp-field "20210110-135222" 'year) "2021"))
  (should (string-equal (mvtn-timestamp-field "20210110-135222" 'month) "01"))
  (should (string-equal (mvtn-timestamp-field "20210110-135222" 'day) "10"))
  (should (string-equal (mvtn-timestamp-field "20210110-135222" 'hour) "13"))
  (should (string-equal (mvtn-timestamp-field "20210110-135222" 'minute) "52"))
  (should (string-equal (mvtn-timestamp-field "20210110-135222" 'second) "22")))


(ert-deftest mvtn-test-substitute-template ()
  "Test `mvtn-substitute-template'"
  (should (string-equal (mvtn-substitute-template
                         "{timestamp} {title} {date}"
                         "mytitle" "mydate" "mytimestamp")
                        "mytimestamp mytitle mydate")))


(ert-deftest mvtn-test-create-new-file ()
  "Test `mvtn-create-new-file'"
  (mvtn-test-with-testfiles
   nil
   (let ((yeardir (concat (mvtn-expand-note-name "prv/zettelkasten") "/"
                          (format-time-string "%Y"))))
     (mvtn-create-new-file "00000000-000000"
                           "prv/zettelkasten" "My Note Title" "test" '("tag1" "tag2") "")
     (should (mvtn-test-file-exists-disregarding-timestamp-p
              "My Note Title -- tag1 tag2.test"
              yeardir))
     (should (not (mvtn-test-file-exists-disregarding-timestamp-p
                   "My Note Title -- tag1 tag2.org"
                   yeardir))))))

(ert-deftest mvtn--extract-note-identity ()
  "Test `mvtn--extract-note-identity'"
  (should-error (mvtn--extract-note-identity "static/asdslkasjdlkj.org"))
  (should-error (mvtn--extract-note-identity "static/2020-01010.org"))
  (should (string-equal (mvtn--extract-note-identity
                         "2020/20200101-010101 test -- tag1 tag2.org")
                        "20200101-010101"))
  (should (string-equal (mvtn--extract-note-identity
                         "2020/20200101-010101 test.org")
                        "20200101-010101"))
  (should (string-equal (mvtn--extract-note-identity
                         "2020/20200101-010101 test name -- tag1 tag2.org" t)
                        "20200101-010101 test name"))
  (should (string-equal (mvtn--extract-note-identity
                         "2020/20200101-010101 test name.txt" t)
                        "20200101-010101 test name"))

  (should (string-equal (mvtn--extract-note-identity
                         "^^2020/20200101-010101 test name.txt^^" t)
                        "20200101-010101 test name"))
  (should (string-equal (mvtn--extract-note-identity
                         "^^2020/20200101-010101 test name -- tag1 tag2.txt^^" t)
                        "20200101-010101 test name"))
  (should (string-equal (mvtn--extract-note-identity
                         "^^20200101-010101 test name    -- tag1    ^^    " t)
                        "20200101-010101 test name"))
  (should (string-equal (mvtn--extract-note-identity
                         "^^20200101-010101 test name^^  " t)
                        "20200101-010101 test name"))
  (should (string-equal (mvtn--extract-note-identity
                         "2020/20200101-010101 ..with.dot.in.name -- tag1 tag2.org")
                        "20200101-010101"))
  (should (string-equal (mvtn--extract-note-identity
                         "2020/20200101-010101 ..with.dot.in.name -- tag1 tag2.org" t)
                        "20200101-010101 ..with.dot.in.name")))


(ert-deftest mvtn-list-files-native ()
  "Test `mvtn-list-files': native (emacs)"
  (let ((mvtn-list-files-function 'mvtn-list-files-function-native))
    (should (string-equal
             (mapconcat 'identity
                        (mvtn-test-with-testfiles nil (mvtn-list-files)) "\n")
             "prv/zettelkasten/2021/20210110-134524 test3 test3.org
prv/zettelkasten/2021/20210110-134523 test2 test2.txt
prv/zettelkasten/2021/20210110-134522 test1 -- i have tags.md
prv/zettelkasten/2020/20201212-134544 test3 test3.org
prv/zettelkasten/2020/20201212-134542 test2 test2 -- tags tags tags.txt
prv/zettelkasten/2020/20201212-134541 test1.txt
prv/zettelkasten/2020/20201212-134541 test1 (fake conflicted copy).txt
wrk/meetings/2019/20190210-134522 a note for work 2.org
wrk/meetings/2019/20190210-134522 a note for work 1.md
prv/static/20130210-134522 an old statically displayed note.org
prv/static/20130210-134522 an old statically displayed note.md"))
    (should (string-equal
             (mapconcat 'identity
                        (mvtn-test-with-testfiles nil (mvtn-list-files t)) "\n")
             "prv/zettelkasten/2021/20210110-134524 test3 test3.org
prv/zettelkasten/2021/20210110-134523 test2 test2.txt
prv/zettelkasten/2021/20210110-134522 test1 -- i have tags.md
prv/zettelkasten/2020/20201212-134544 test3 test3.org
prv/zettelkasten/2020/20201212-134542 test2 test2 -- tags tags tags.txt
prv/zettelkasten/2020/20201212-134541 test1.txt
prv/zettelkasten/2020/20201212-134541 test1 (fake conflicted copy).txt
wrk/meetings/2019/20190210-134522 a note for work 2.org
wrk/meetings/2019/20190210-134522 a note for work 1.md
prv/zettelkasten/2018/20181212-134544 test3 test3.org
prv/zettelkasten/2018/20181212-134542 test2 test2 -- tags tags tags.txt
prv/zettelkasten/2018/20181212-134541 test1.txt
prv/zettelkasten/1999/19990110-134523 test3 test3.org
prv/zettelkasten/1999/19990110-134522 test2 test2.txt
prv/zettelkasten/1999/19990110-134522 test1 -- tags test.txt
prv/static/20130210-134522 an old statically displayed note.org
prv/static/20130210-134522 an old statically displayed note.md"))))

(ert-deftest mvtn-list-files-find ()
  "Test `mvtn-list-files': GNU find"
  (let ((mvtn-list-files-function 'mvtn-list-files-function-find))
    (should (string-equal
             (mapconcat 'identity
                        (mvtn-test-with-testfiles nil (mvtn-list-files)) "\n")
             "prv/zettelkasten/2021/20210110-134524 test3 test3.org
prv/zettelkasten/2021/20210110-134523 test2 test2.txt
prv/zettelkasten/2021/20210110-134522 test1 -- i have tags.md
prv/zettelkasten/2020/20201212-134544 test3 test3.org
prv/zettelkasten/2020/20201212-134542 test2 test2 -- tags tags tags.txt
prv/zettelkasten/2020/20201212-134541 test1.txt
prv/zettelkasten/2020/20201212-134541 test1 (fake conflicted copy).txt
wrk/meetings/2019/20190210-134522 a note for work 2.org
wrk/meetings/2019/20190210-134522 a note for work 1.md
prv/static/20130210-134522 an old statically displayed note.org
prv/static/20130210-134522 an old statically displayed note.md"))
    (should (string-equal
             (mapconcat 'identity
                        (mvtn-test-with-testfiles nil (mvtn-list-files t)) "\n")
             "prv/zettelkasten/2021/20210110-134524 test3 test3.org
prv/zettelkasten/2021/20210110-134523 test2 test2.txt
prv/zettelkasten/2021/20210110-134522 test1 -- i have tags.md
prv/zettelkasten/2020/20201212-134544 test3 test3.org
prv/zettelkasten/2020/20201212-134542 test2 test2 -- tags tags tags.txt
prv/zettelkasten/2020/20201212-134541 test1.txt
prv/zettelkasten/2020/20201212-134541 test1 (fake conflicted copy).txt
wrk/meetings/2019/20190210-134522 a note for work 2.org
wrk/meetings/2019/20190210-134522 a note for work 1.md
prv/zettelkasten/2018/20181212-134544 test3 test3.org
prv/zettelkasten/2018/20181212-134542 test2 test2 -- tags tags tags.txt
prv/zettelkasten/2018/20181212-134541 test1.txt
prv/zettelkasten/1999/19990110-134523 test3 test3.org
prv/zettelkasten/1999/19990110-134522 test2 test2.txt
prv/zettelkasten/1999/19990110-134522 test1 -- tags test.txt
prv/static/20130210-134522 an old statically displayed note.org
prv/static/20130210-134522 an old statically displayed note.md"))))


(ert-deftest mvtn-test-link-targets ()
  "Test `mvtn-link-targets'"
  (mvtn-test-with-testfiles nil
   (should (mvtn-link-targets "^^20210110-134524 test3 test3.org^^"))
   (should (mvtn-link-targets "^^20210110-134524 test3 test3^^"))
   (should (mvtn-link-targets "^^20210110-134524^^"))
   (should (not (mvtn-link-targets "^^20210110-134525^^")))
   (should (mvtn-link-targets "^^19990110-134522^^"))
   (should (eq 2 (length (mvtn-link-targets "^^20201212-134541 test1.txt^^"))))
   (should (mvtn-link-targets "^^20130210-134522 an old statically displayed note^^"))
   (should (eq 2 (length (mvtn-link-targets "^^20190210-134522 a note for work^^"))))
   (should-error (mvtn-link-targets "^^20210110-1345^^"))
   (should (not (mvtn-link-targets
                 "^^20130210-123456 a note in an excluded folder^^")))))


(ert-deftest mvtn-test-link-action-search ()
  "Test `mvtn-link-actions' and `mvtn-link-action-search'"
  (mvtn-test-with-testfiles nil
   (mvtn-follow-link "^^20181212-134541^^")
   (mvtn-follow-link "^^20181212-134541 :: content^^")
   (should (string-equal (word-at-point) "content"))
   (mvtn-follow-link "^^20181212-134541 title :: content^^")
   (should (string-equal (word-at-point) "content"))
   (mvtn-follow-link "^^20181212-134541 :: some^^")
   (should (string-equal (word-at-point) "some"))
   (kill-buffer "20181212-134541 test1.txt")))


;; TODO Unit tests for search.
;; I am unsure how to test search. One would have to somehow wait for the result
;; of the search command (grep, etc) and only then search the result buffer
;; contents. I do not know if/how that could be achieved.

(provide 'mvtn-test)

;;; mvtn-test.el ends here
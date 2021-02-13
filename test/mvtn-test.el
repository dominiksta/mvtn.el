;;; mvtn-test.el --- Unit tests for mvtn -*- lexical-binding: t -*-

(require 'mvtn)
(require 'mvtn-test-helpers)
(require 'ert)


;; NOTE: This is a personal project, not "enterprise" code. I did not invest
;; time in good coverage or anything of the sort. These are just basic tests to
;; (hopefully) catch basic bugs.


(defvar mvtn-test-note-dir (expand-file-name "test-notes")
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


(ert-deftest mvtn-test-get-create-current-note-directory ()
  "Test `mvtn-get-create-current-year-directory'"
  (delete-directory mvtn-test-note-dir t)
  (let ((mvtn-note-directory mvtn-test-note-dir))
    (should (not (file-exists-p mvtn-note-directory)))
    (should (string-equal (mvtn-get-create-current-year-directory)
                          (format "%s/%s" mvtn-note-directory
                                  (format-time-string "%Y"))))
    (should (file-exists-p mvtn-note-directory))
    (should (string-equal (mvtn-get-create-current-year-directory)
                          (format "%s/%s" mvtn-note-directory
                                  (format-time-string "%Y"))))
    (should (file-exists-p mvtn-note-directory))))


(ert-deftest mvtn-test-create-new-file ()
  "Test `mvtn-create-new-file'"
  (delete-directory mvtn-test-note-dir t)
  (let ((mvtn-note-directory mvtn-test-note-dir)
        (mvtn-default-file-extension "test"))
    (mvtn-create-new-file "My Note Title" "tag1 tag2")
    (should (mvtn-test-file-exists-disregarding-timestamp-p
             "My Note Title -- tag1 tag2.test"
             (mvtn-get-create-current-year-directory)))
    (should (not (mvtn-test-file-exists-disregarding-timestamp-p
                  "My Note Title -- tag1 tag2.org"
                  (mvtn-get-create-current-year-directory))))))

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
                        "20200101-010101 test name")))


(ert-deftest mvtn-list-files ()
  "Test `mvtn-list-files'"
  (should (string-equal
           (mapconcat 'identity
                      (mvtn-test-with-testfiles (mvtn-list-files)) "\n")
           "2021/20210110-134524 test3 test3.org
2021/20210110-134523 test2 test2.txt
2021/20210110-134522 test1 -- i have tags.md
2020/20201212-134544 test3 test3.org
2020/20201212-134542 test2 test2 -- tags tags tags.txt
2020/20201212-134541 test1.txt
2020/20201212-134541 test1 (fake conflicted copy).txt
static/20130210-134522 an old statically displayed note.org
static/20130210-134522 an old statically displayed note.md
static/work/20140210-134522 a note for work 2.org
static/work/20140210-134522 a note for work 1.md"))
  (should (string-equal
           (mapconcat 'identity
                      (mvtn-test-with-testfiles (mvtn-list-files t)) "\n")
           "2021/20210110-134524 test3 test3.org
2021/20210110-134523 test2 test2.txt
2021/20210110-134522 test1 -- i have tags.md
2020/20201212-134544 test3 test3.org
2020/20201212-134542 test2 test2 -- tags tags tags.txt
2020/20201212-134541 test1.txt
2020/20201212-134541 test1 (fake conflicted copy).txt
2018/20181212-134544 test3 test3.org
2018/20181212-134542 test2 test2 -- tags tags tags.txt
2018/20181212-134541 test1.txt
1999/19990110-134523 test3 test3.org
1999/19990110-134522 test2 test2.txt
1999/19990110-134522 test1 -- tags test.txt
static/20130210-134522 an old statically displayed note.org
static/20130210-134522 an old statically displayed note.md
static/work/20140210-134522 a note for work 2.org
static/work/20140210-134522 a note for work 1.md")))


(ert-deftest mvtn-test-link-targets ()
  "Test `mvtn-link-targets'"
  (mvtn-test-with-testfiles
   (should (mvtn-link-targets "^^20210110-134524 test3 test3.org^^"))
   (should (mvtn-link-targets "^^20210110-134524 test3 test3^^"))
   (should (mvtn-link-targets "^^20210110-134524^^"))
   (should (not (mvtn-link-targets "^^20210110-134525^^")))
   (should (mvtn-link-targets "^^19990110-134522^^"))
   (should (eq 2 (length (mvtn-link-targets "^^20201212-134541 test1.txt^^"))))
   (should (mvtn-link-targets "^^20130210-134522 an old statically displayed note^^"))
   (should (eq 2 (length (mvtn-link-targets "^^20140210-134522 a note for work^^"))))
   (should-error (mvtn-link-targets "^^20210110-1345^^"))
   (should (not (mvtn-link-targets
                 "^^20130210-123456 a note in an excluded folder^^")))))


(ert-deftest mvtn-test-link-action-search ()
  "Test `mvtn-link-actions' and `mvtn-link-action-search'"
  (mvtn-test-with-testfiles
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


(ert-deftest mvtn-test-rename-current-file ()
  "Test `mvtn-rename-current-file'"
  (mvtn-test-with-testfiles
   (let ((orig-file (concat mvtn-test-note-dir
                            "/1999/19990110-134522 test2 test2.txt"))
         (should-new-file (concat mvtn-test-note-dir
                                  "/1999/19990110-134522 My New Name.txt")))
     (with-current-buffer (find-file-noselect orig-file)
       (should (string-equal (buffer-substring-no-properties
                              (point-min) (point-at-eol))
                             "title: test2 test2"))
       (buffer-substring-no-properties (point-min) (point-at-eol))
       (mvtn-rename-current-file "My New Name")
       (should (not (file-exists-p orig-file)))
       (should (file-exists-p should-new-file))
       (should (string-equal (buffer-substring-no-properties
                              (point-min) (point-at-eol))
                             "title: My New Name"))
       (kill-buffer)))))



;;; mvtn-test.el ends here
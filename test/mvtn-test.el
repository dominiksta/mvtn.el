;;; mvtn-test.el --- summary -*- lexical-binding: t -*-

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


(ert-deftest mvtn-list-files ()
  "Test `mvtn-list-files'"
  (should (string-equal
           (mapconcat 'identity
                      (mvtn-test-with-testfiles (mvtn-list-files)) "\n")
           "20210110-134522 test1 -- i have tags.md
20210110-134523 test2 test2.txt
20210110-134524 test3 test3.org
20201212-134541 test1.txt
20201212-134542 test2 test2 -- tags tags tags.txt
20201212-134544 test3 test3.org")))



;;; mvtn-test.el ends here
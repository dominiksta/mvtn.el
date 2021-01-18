;;; mvtn-test.el --- summary -*- lexical-binding: t -*-

(require 'mvtn)
(require 'ert)

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

;;; mvtn-test.el ends here
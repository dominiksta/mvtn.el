;;; mvtn-test-journal.el --- Unit tests for mvtn-journal.el -*- lexical-binding: t -*-

;;; Code:

(require 'mvtn)
(require 'mvtn-journal)

(ert-deftest mvtn-test-journal-new-entry-for-time ()
  "Test `mvtn-journal-new-entry-for-time'"
  (let ((time1 (encode-time 12 30 10 30 09 2021))
        (time2 (encode-time 30 30 10 30 09 2021))
        (time3 (encode-time 31 30 11 30 09 2021))
        (inhibit-message t))
    (mvtn-test-with-testfiles nil
     ;; Before opening the file should not exist
     (should (not (mvtn-link-targets "^^20210930-000000^^")))
     (mvtn-journal-new-entry-for-time time1 "hi")
     ;; Opening the first time should create the file and insert an entry with
     ;; the first time
     (should (eq (length (mvtn-link-targets "^^20210930-000000^^")) 1))
     (goto-char (point-min))
     (should (eq 1 (count-matches "^* /..:../")))
     (should (eq 1 (count-matches "^* /10:30/ hi")))

     ;; Opening the second time should not create a file
     (mvtn-journal-new-entry-for-time time2 "hi")
     (should (eq (length (mvtn-link-targets "^^20210930-000000^^")) 1))
     (goto-char (point-min))
     (should (eq 2 (count-matches "^* /..:../")))
     (should (eq 2 (count-matches "^* /10:30/ hi")))

     (mvtn-journal-new-entry-for-time time3 "lol")
     (should (eq (length (mvtn-link-targets "^^20210930-000000^^")) 1))
     (goto-char (point-min))
     (should (eq 3 (count-matches "^* /..:../")))
     (should (eq 2 (count-matches "^* /10:30/ hi")))
     (should (eq 1 (count-matches "^* /11:30/ lol"))))))

(provide 'mvtn-test-journal)

;;; mvtn-test-journal.el ends here
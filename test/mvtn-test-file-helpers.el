;;; mvtn-test-file-helper.el --- Unit tests for mvtn-file-helpers -*- lexical-binding: t -*-

(require 'mvtn)
(require 'mvtn-test)
(require 'mvtn-file-helpers)

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

;;; mvtn-test-file-helpers.el ends here
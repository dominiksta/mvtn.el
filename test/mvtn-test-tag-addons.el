;;; mvtn-test-tag-addons.el --- Unit tests for mvtn-file-helpers -*- lexical-binding: t -*-

(require 'mvtn)
(require 'mvtn-test)
(require 'mvtn-tag-addons)

(ert-deftest mvtn-test-list-files-with-tags ()
  "Test `mvtn-list-files-with-tags'"
  (should (string-equal
           (mapconcat 'identity
                      (mvtn-test-with-testfiles
                       nil (mvtn-list-files-with-tags "have")) "\n")
           "prv/zettelkasten/2021/20210110-134522 test1 -- i have tags.md"))
  (should (string-equal
           (mapconcat 'identity
                      (mvtn-test-with-testfiles
                       nil (mvtn-list-files-with-tags "tags")) "\n")
           "prv/zettelkasten/2021/20210110-134522 test1 -- i have tags.md
prv/zettelkasten/2020/20201212-134542 test2 test2 -- tags tags tags.txt"))
  (should (string-equal
           (mapconcat 'identity
                      (mvtn-test-with-testfiles
                       nil (mvtn-list-files-with-tags "tags" t)) "\n")
           "prv/zettelkasten/2021/20210110-134522 test1 -- i have tags.md
prv/zettelkasten/2020/20201212-134542 test2 test2 -- tags tags tags.txt
prv/zettelkasten/2018/20181212-134542 test2 test2 -- tags tags tags.txt
prv/zettelkasten/1999/19990110-134522 test1 -- tags test.txt")))

(provide 'mvtn-test-tag-addons)

;;; mvtn-test-file-helpers.el ends here

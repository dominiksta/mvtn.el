;;; mvtn-test-templates.el --- Unit tests for mvtn templates -*- lexical-binding: t -*-

(require 'mvtn)
(require 'mvtn-test)
(require 'mvtn-templates)

;; These tests will have to be adapted when the default templates change. I feel
;; that it is simply to much effort to mock template files right now.

(ert-deftest mvtn-test-template-listings ()
  "Test `mvtn-template-list-short'. Implicitly also kind of tests
`mvtn-template-list' and the default value of
`mvtn-template-locations'."
  (let ((templates-found (mvtn-template-list-short)))
    (should (eq (length templates-found) 2))
    (should (member "literature" templates-found))
    (should (member "meeting" templates-found))))

(ert-deftest mvtn-test-template-expand-short-file-name ()
  "Test `mvtn-template-expand-short-file-name'"
  (should (string-match-p "meeting.json$"
                          (mvtn-template-expand-short-file-name "meeting")))
  (should (string-match-p "literature.json$"
                          (mvtn-template-expand-short-file-name "literature")))
  (should-error (mvtn-template-expand-short-file-name "literatur"))
  (should-error (mvtn-template-expand-short-file-name "hi")))

(ert-deftest mvtn-test-json-parse-string ()
  "Test `mvtn-json-parse-string'"
  (should (mvtn-json-parse-string "{}"))
  (should-error (mvtn-json-parse-string "{")))

(ert-deftest mvtn-test-template-parse-json-file ()
  "(Very) rudimentary test for `mvtn-template-parse-json-file'"
  (let ((meeting (mvtn-template-parse-json-file
                  (mvtn-template-expand-short-file-name "meeting")))
        (literature (mvtn-template-parse-json-file
                     (mvtn-template-expand-short-file-name "literature"))))
    (should (string-equal (alist-get 'meta.format meeting) "mvtn-template"))
    (should (eq (alist-get 'meta.version meeting) 1))
    (dolist (el '(meta.format meta.version file.short_note_dir file.title
                              file.title_prompt file.tags file.tags_prompt
                              file.template_file))
      (when (not (alist-get el meeting))
        (error "Missing key: %s" (symbol-name el)))
      (when (not (alist-get el literature))
        (error "Missing key: %s" (symbol-name el))))))

;; TODO: In order to test mvtn-new-note-from-template and other interactive
;; functions, the package 'with-simulated-input'
;; (https://github.com/DarwinAwardWinner/with-simulated-input) could be used. To
;; do that, some system would need to be in place to declare development
;; dependencies though.

(provide 'mvtn-test-templates)

;;; mvtn-test-templates.el ends here
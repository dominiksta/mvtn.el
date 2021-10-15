;;; mvtn-test-backlink-buffer.el --- Tests for the backlink buffer -*- lexical-binding: t -*-

;;; Code:

(require 'mvtn)
(require 'mvtn-backlink-buffer)

(ert-deftest mvtn-test-search-async-command ()
  "Test `mvtn-search-async-command'."
  (mvtn-test-with-testfiles
   nil
   ;; TODO: Also test `mvtn-search-full-text-rg-async-command'. For that I would
   ;; need some sort of system for development dependencies though and adapt the
   ;; github actions workflow to install rg.
   ;; For now this should not be too much of an issue since the defined rg
   ;; command seems to produce exactly the same output as the grep command. Also
   ;; I (@dominiksta) use the rg version daily so if there is an error I should
   ;; notice.
   (dolist (func '(mvtn-search-full-text-grep-async-command))
     (let ((cmd (funcall func "hi test")))
       (should (member "hi test" cmd))
       (dolist (dir (mvtn--search-dirs))
        (should (member dir cmd)))))))

(ert-deftest mvtn-backlink-buffer-parse-command-output ()
  "Test `mvtn-backlink-buffer-parse-command-output'."
  (mvtn-test-with-testfiles
   nil
   (dolist (func '(mvtn-search-full-text-grep-async-command))
     (let* ((cmd (mapconcat 'shell-quote-argument
                            (funcall func "\\^\\^20210110-134524") " "))
            (default-directory (plist-get (car mvtn-note-directories) :dir))
            (parsed (mvtn-backlink-buffer-parse-command-output
                     (shell-command-to-string cmd))))
       ;; test amount of results
       (should (eq (length parsed) 2))
       ;; test search result file
       (should (string-match-p "20201212-134544" (plist-get (car parsed) :file)))
       ;; test search result line
       (should (string-match-p "Here is a link" (plist-get (cadr parsed) :text)))
       ;; test context
       (should (string-match-p "---------" (plist-get (cadr parsed) :text)))))))
  
(provide 'mvtn-test-backlink-buffer)

;;; mvtn-test-backlink-buffer.el ends here

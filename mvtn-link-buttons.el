;;; mvtn-link-buttons.el --- Buttonize Mvtn Links -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to buttonize
;; links, making them clickable and hiding a links id to make it more legible.

;;; Code:

(require 'mvtn)

(defface mvtn-link-face `((t :inherit link))
  "Face used for displaying mvtn links." :group 'mvtn)
(defface mvtn-link-mouse-face `((t :inherit highlight))
  "Face used when hovering over mvtn links." :group 'mvtn)

(defvar mvtn-link-map (make-sparse-keymap)
  "The keymap when point is at an mvtn")

(defvar mvtn--named-link-regexp
  "\\(\\^\\^\\)\\([[:digit:]]\\{8\\}-[[:digit:]]\\{6\\}\\)\\( \\)\\(.+\\)\\(\\^\\^\\)"
  "Only matches 'named' mvtn links (meaning links with text after the id)")

(defvar mvtn-link-properties
  `(face mvtn-link-face mouse-face mvtn-link-mouse-face
         keymap ,mvtn-link-map button-lock t)
  "The font-lock properties of an mvtn link.")

(defvar mvtn-link-fl-keywords
  `(;; the entire link
    (,mvtn--link-regexp (0 ',mvtn-link-properties prepend))
    ;; opening carets
    (,mvtn--named-link-regexp (1 `(face default invisible t) prepend))
    ;; id
    (,mvtn--named-link-regexp (2 `(face default invisible t) prepend))
    ;; space after id in named note
    (,mvtn--named-link-regexp (3 `(face default invisible t) prepend))
    ;; closing carets
    (,mvtn--named-link-regexp (5 `(face default invisible t) prepend)))
  "The font-lock keywords for different parts of mvtn links.")


(defun mvtn-link-buttons-follow-mouse (event &optional promote-to-region)
  "Follow an Mvtn link at the clicked position. Must be bound to
a mouse button for EVENT to be passed in correctly."
  (interactive "e")
  (mouse-set-point event promote-to-region)
  (deactivate-mark)
  (mvtn-follow-link-at-point))

(defun mvtn-link-buttons-fontify ()
  "Make mvtn links in the current buffer clickable and fontify
them with `mvtn-link-face' and `mvtn-link-mouse-face'."
  (font-lock-add-keywords nil mvtn-link-fl-keywords)
  (font-lock-flush))

(defun mvtn-link-buttons-defontify ()
  "Undo `mvtn-link-buttons-fontify'."
  (font-lock-remove-keywords nil mvtn-link-fl-keywords)
  (font-lock-flush))

(defun mvtn-link-button-edit ()
  "Edit the name of the mvtn link button under point."
  (interactive)
  (save-match-data
    (save-excursion
      ;; Move to the beginning of the link
      (cond ((looking-at "\\^\\^[[:digit:]]") nil)
            ((looking-at "\\^[[:digit:]]") (backward-char))
            (t (search-backward "^^" (point-at-bol) t)))
      (when (not (looking-at-p mvtn--link-regexp)) (error "No link under point"))
      (if (looking-at mvtn--named-link-regexp)
          (let* ((id (substring-no-properties (match-string 2)))
                 (name (substring-no-properties (match-string 4)))
                 (point-end (match-end 0))
                 (newid (read-from-minibuffer "Link: " id))
                 (newname (read-from-minibuffer "Description: " name)))
            (delete-region (point) point-end)
            (insert "^^" newid " " newname "^^"))
        (error "No named link under point")))))


(define-key mvtn-link-map (kbd "RET") 'mvtn-follow-link-at-point)
(define-key mvtn-link-map (kbd "C-c C-l") 'mvtn-link-button-edit)
(define-key mvtn-link-map (kbd "<mouse-2>") 'mvtn-link-buttons-follow-mouse)
(when mouse-1-click-follows-link
  (define-key mvtn-link-map (kbd "<mouse-1>") 'mvtn-link-buttons-follow-mouse))

(add-hook 'mvtn-minor-mode-hook 'mvtn-link-buttons-fontify)

(provide 'mvtn-link-buttons)
;;; mvtn-link-buttons.el ends here

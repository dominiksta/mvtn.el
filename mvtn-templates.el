;;; mvtn-templates.el --- Templates for mvtn -*- lexical-binding: t -*-

;;; Commentary:

;; This file may be loaded in addition to mvtn's core (mvtn.el) to add support
;; for templates.  See `mvtn-template-locations' for documentation.

;;; Code:

(require 'mvtn)
(require 'mvtn-file-helpers)
(require 'mvtn-tag-addons)
(require 'json)

(defcustom mvtn-template-locations
  (list (expand-file-name
         (concat (or load-file-name buffer-file-name) "/../templates"))
        (expand-file-name
         (concat (plist-get (car mvtn-note-directories) :dir) "/templates")))
  "The locations on the file system for mvtn template files.

An mvtn template is a .json file of the following format:

{
    \"meta\": {
        \"format\": \"mvtn-template\",
        \"version\": 1
    },
    \"file\": {
        \"short_note_dir\": \"Any String\",
        \"title\": \"Any String\",
        \"title_prompt\": [true|false],
        \"tags\": \"Comma-Seperated String of Tags\",
        \"tags_prompt\": [true|false],
        \"template_file\": \"A file in the same directory as this json\"
    }
}

The \"meta\" section is mandatory and must be specified in the
exact same way as shown here.  The \"file\" section is where the
actual template information is stored.

- \"short_note_dir\" refers to a note directory as returned by
  `mvtn-short-note-dir-list'
- \"title\" refers to the title of the note that will be created
- \"title_prompt\" specifies wether mvtn should prompt for the title or simply
  use the value of the \"title\" field
- \"tags\" refers to the tags of the note that will be created.  Multiple tags
  should be seperated by a comma (,)
- \"tags_prompt\" specifies wether mvtn should prompt for the tags or simply
  use the value of the \"tags\" field
- \"template_file\" refers to a file in the same directory as the .json template
  file.  This file will be inserted into the new note.  The following
  substitutions are available:
  - {date} will be replaced by the current date in the format YYYY-MM-DD
  - {timestamp} will be replaced by the timestamp mvtn automatically assigns to
    the new note
  - {title} will be replaced by the notes title see the \"title\" and
    \"title_prompt\" fields above
  - {point} will be deleted and the point aka cursor will be placed in its
    position once the buffer to the new note is opened

For examples of template files, see the 'template' directory in mvtns
repository/package folder.  By default, this directory will be the first element
of this variable.

To actually create a new note from a template, see
`mvtn-new-note-from-template'."
  :type '(list :value-type string) :group 'mvtn)

(defun mvtn-template-list ()
  "List all full file names for all mvtn templates.
Goes through all locations defined in `mvtn-template-locations'."
  (let ((result nil))
    (dolist (dir mvtn-template-locations)
      (when (file-exists-p dir)
        ;; the regexp exlcudes . and ..
        (setq result (append (directory-files
                              dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\).+\\.json")
                             result))))
    result))

(defun mvtn-template-list-short ()
  "RETURN the filenames of all mvtn templates sans extension and directory."
  (mapcar (lambda (f) (file-name-sans-extension (file-name-nondirectory f)))
          (mvtn-template-list)))

(defun mvtn-template-expand-short-file-name (filename)
  "RETURN the full filename for a short FILENAME in `mvtn-template-list-short'."
  (let* ((long-file-names (mvtn-template-list))
         (short-file-names (mvtn-template-list-short)))
    (when (not (member filename short-file-names))
      (error "No mvtn template was found with name %s" filename))
    (let ((resultset (seq-filter (lambda (f) (string-equal (file-name-sans-extension
                                                       (file-name-nondirectory f))
                                                      filename))
                                 long-file-names)))
      (when (> (length resultset) 1)
        (error "Conflicting template names: %s" resultset))
      (car resultset))))

(defun mvtn-json-parse-string (string)
  "Parse a json string STRING.
When an Emacs version < 27 is detected, uses json.el.  Otherwise,
it uses the new native json implementation."
  (if (< emacs-major-version 27)
      (let ((json-object-type 'hash-table))
        (ignore json-object-type)
        (json-read-from-string string))
    (json-parse-string string)))

(defun mvtn-template-parse-json-file (file)
  "Parse an mvtn template json file FILE into an alist.
Will error if FILE is not an mvtn template."
  (let* ((root (mvtn-json-parse-string (mvtn-get-string-from-file file)))
         (meta (gethash "meta" root))
         (file (gethash "file" root))
         (data (list
                (cons 'meta.format (gethash "format" meta))
                (cons 'meta.version (gethash "version" meta))
                (cons 'file.short_note_dir (gethash "short_note_dir" file))
                (cons 'file.title (gethash "title" file))
                (cons 'file.title_prompt (gethash "title_prompt" file))
                (cons 'file.tags (gethash "tags" file))
                (cons 'file.tags_prompt (gethash "tags_prompt" file))
                (cons 'file.template_file (gethash "template_file" file)))))
    (dolist (el data)
      (when (not (cdr el)) (error "Missing key: %s" (symbol-name (car el)))))
    (when (not (string-equal (alist-get 'meta.format data) "mvtn-template"))
      (error "Could not find file-format header"))
    (when (not (eq (alist-get 'meta.version data) 1))
      (error "Unsupported version: %s" (alist-get 'meta-version data)))
    data))

;;;###autoload
(defun mvtn-new-note-from-template (&optional encrypt)
  "Prompt for a template and create a note from it.
The templates are selected from `mvtn-template-locations'.
Depending on how the template was created, you might be prompted
for a title or tags.  If optional ENCRYPT is non-nil (can be set
with a universal argument), the file will be encrypted with gpg."
  (interactive "P")
  (let* ((answer (completing-read "Template: " (mvtn-template-list-short)))
         (full-name (mvtn-template-expand-short-file-name answer))
         (template (mvtn-template-parse-json-file full-name))
         (template-file (expand-file-name
                         (concat (file-name-directory full-name)
                                 (alist-get 'file.template_file template))))

         (title (if (not (eq :false (alist-get 'file.title_prompt template)))
                    (read-from-minibuffer "Title: "
                                          (alist-get 'file.title template))
                  (alist-get 'file.title template)))

         (tags (if (not (eq :false (alist-get 'file.tags_prompt template)))
                   (if mvtn-cv-enable
                       (mvtn-cv-prompt-for-tags (alist-get 'file.tags template))
                     (mvtn-prompt-for-tags (alist-get 'file.tags template)))
                 (split-string (alist-get 'file.tags template) ",")))

         (timestamp (mvtn-current-timestamp 'second))
         (content (mvtn-substitute-template
                   (mvtn-get-string-from-file template-file)
                   title (format-time-string "%Y-%m-%d") timestamp)))
    (switch-to-buffer
     (mvtn-create-new-file
      timestamp (alist-get 'file.short_note_dir template) title
      (file-name-extension template-file) tags content encrypt))))

(provide 'mvtn-templates)

;;; mvtn-templates.el ends here
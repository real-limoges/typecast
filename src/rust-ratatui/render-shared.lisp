(in-package #:typecast/rust-ratatui)

;;;; Pass 2b: Render shared/static Rust files
;;;;
;;;; Static .rs templates live in src/rust-ratatui/templates/.
;;;; They are read at load time and baked into the SBCL binary by save-lisp-and-die.

(defun %template-path (filename)
  "Resolve FILENAME relative to the backend's templates/ directory."
  (merge-pathnames (concatenate 'string "src/rust-ratatui/templates/" filename)
                   (asdf:system-source-directory :typecast)))

(defvar *traits-rs*
  (alexandria:read-file-into-string (%template-path "traits.rs"))
  "Contents of traits.rs — FormComponent trait and EventResult enum.")

(defvar *primitives-rs*
  (alexandria:read-file-into-string (%template-path "primitives.rs"))
  "Contents of primitives.rs — primitive widget implementations.")

(defvar *list-editor-rs*
  (alexandria:read-file-into-string (%template-path "list_editor.rs"))
  "Contents of list_editor.rs — ListEditor widget.")

(defvar *kv-editor-rs*
  (alexandria:read-file-into-string (%template-path "kv_editor.rs"))
  "Contents of kv_editor.rs — KvEditor widget.")

(defvar *optional-field-rs*
  (alexandria:read-file-into-string (%template-path "optional_field.rs"))
  "Contents of optional_field.rs — OptionalField wrapper.")

(defvar *root-form-template*
  (alexandria:read-file-into-string (%template-path "root_form.rs.template"))
  "Template for root_form.rs — {{ROOT_NAME}} and {{ROOT_FILE}} are substituted at emit time.")

(defun replace-placeholders (template alist)
  "Replace {{KEY}} markers in TEMPLATE with values from ALIST of (\"KEY\" . \"value\") pairs."
  (dolist (pair alist template)
    (let* ((marker (format nil "{{~A}}" (car pair)))
           (value  (cdr pair))
           (mlen   (length marker)))
      (setf template
            (with-output-to-string (out)
              (loop with pos = 0
                    for found = (search marker template :start2 pos)
                    while found
                    do (write-string template out :start pos :end found)
                       (write-string value out)
                       (setf pos (+ found mlen))
                    finally (write-string template out :start pos)))))))

(defun render-traits-rs ()
  "Emit traits.rs — the FormComponent trait and EventResult enum."
  *traits-rs*)

(defun render-primitives-rs ()
  "Emit primitives.rs — primitive widget implementations (TextInput, NumericInput, etc.)."
  *primitives-rs*)

(defun render-list-editor-rs ()
  "Emit list_editor.rs — the ListEditor widget for Vec<T> fields."
  *list-editor-rs*)

(defun render-kv-editor-rs ()
  "Emit kv_editor.rs — the KvEditor widget for HashMap<String, V> fields."
  *kv-editor-rs*)

(defun render-optional-field-rs ()
  "Emit optional_field.rs — the OptionalField wrapper for Option<T> fields."
  *optional-field-rs*)

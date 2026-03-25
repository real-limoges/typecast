(in-package #:typecast/rust-ratatui)

;;;; Entry point (library API)

(defun generate (schema-path output-dir)
  "Main entry point: parse schema, validate, sort, and emit generated Rust files.
   SCHEMA-PATH is a pathname or string to config-schema.json.
   OUTPUT-DIR is a pathname or string to the target directory (e.g. src/generated/)."
  (let* ((schema-path (pathname schema-path))
         (output-dir (let ((d (pathname output-dir)))
                       ;; Ensure trailing slash
                       (if (pathname-name d)
                           (make-pathname :directory (append (pathname-directory d)
                                                             (list (pathname-name d)))
                                          :defaults d)
                           d))))
    (format t "typecast: parsing ~A~%" schema-path)
    (let ((schema (parse-schema schema-path)))
      (format t "typecast: found ~D types, root = ~A~%"
              (hash-table-count (schema-types schema))
              (schema-root schema))

      ;; Validate
      (format t "typecast: resolving named references...~%")
      (resolve-named-refs schema)

      ;; Topological sort
      (format t "typecast: topological sort...~%")
      (let ((sorted (topological-sort-types schema)))
        (format t "typecast: generation order:~%")
        (dolist (pair sorted)
          (format t "  ~A (~A)~%" (car pair) (type-def-kind (cdr pair))))

        ;; Emit
        (format t "typecast: emitting to ~A~%" output-dir)
        (emit-generated-files schema sorted output-dir)

        (format t "typecast: done (~D files written)~%"
                (+ +non-type-file-count+ (length sorted)))

        schema))))

;;;; CLI entry point

(defun print-usage ()
  "Print command-line usage information to *error-output*."
  (format *error-output* "Usage: typecast <schema.json> <output-dir/>~%")
  (format *error-output* "~%")
  (format *error-output* "  Reads a JSON schema and generates Rust/ratatui TUI source files.~%")
  (format *error-output* "~%")
  (format *error-output* "Arguments:~%")
  (format *error-output* "  schema.json   Path to the config schema JSON file~%")
  (format *error-output* "  output-dir/   Directory to write generated .rs files into~%"))

(defun cli-main ()
  "CLI entry point for the standalone binary."
  (let ((args (uiop:command-line-arguments)))
    ;; Note: --help is intercepted by the SBCL runtime in standalone
    ;; executables, so we only check -h here.  No-args also prints usage.
    (when (member "-h" args :test #'string=)
      (print-usage)
      (uiop:quit 0))
    (unless (= (length args) 2)
      (print-usage)
      (uiop:quit 1))
    (let ((schema-path (first args))
          (output-dir (second args)))
      (unless (uiop:file-exists-p schema-path)
        (format *error-output* "Error: schema file not found: ~A~%" schema-path)
        (uiop:quit 1))
      (handler-case
          (progn
            (generate schema-path output-dir)
            (uiop:quit 0))
        (error (e)
          (format *error-output* "Error: ~A~%" e)
          (uiop:quit 1))))))

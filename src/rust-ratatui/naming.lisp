(in-package #:typecast/rust-ratatui)

;;;; Rust naming conventions

(defun to-pascal-case (s)
  "Convert a variant value to PascalCase.
   \"constantRpm\" -> \"ConstantRpm\"
   \"GET\" -> \"Get\" (all-caps special case)."
  (cond
    ((zerop (length s)) "")
    ;; All-caps like "GET" -> "Get"
    ((let ((alpha (remove-if-not #'alpha-char-p s)))
       (and (plusp (length alpha))
            (every #'upper-case-p alpha)))
     (let ((lower (string-downcase s)))
       (concatenate 'string
                    (string (char-upcase (char lower 0)))
                    (subseq lower 1))))
    ;; Normal camelCase or lowercase: uppercase first letter
    (t (concatenate 'string
                    (string (char-upcase (char s 0)))
                    (subseq s 1)))))

(defun type-name-to-filename (name)
  "Convert a schema type name to a Rust source file name (without extension).
   \"HealthCheckConfig\" -> \"health_check_config\"."
  (camel-to-snake name))

(defparameter *rust-keywords*
  '("as" "break" "const" "continue" "crate" "dyn" "else" "enum"
    "extern" "false" "fn" "for" "if" "impl" "in" "let" "loop"
    "match" "mod" "move" "mut" "pub" "ref" "return" "self"
    "static" "struct" "super" "trait" "true" "type" "unsafe"
    "use" "where" "while")
  "Rust reserved keywords that need r# prefix when used as identifiers.")

(defun field-rust-name (field-def)
  "Compute the Rust identifier for a field-def, escaping Rust keywords as needed."
  (rust-field-name (camel-to-snake (field-def-key field-def))))

(defun rust-field-name (snake)
  "Escape a snake_case field name if it collides with a Rust keyword.
   \"type\" -> \"r#type\", \"name\" -> \"name\"."
  (if (member snake *rust-keywords* :test #'string=)
      (concatenate 'string "r#" snake)
      snake))

(defun needs-rename-p (key)
  "Return T when the JSON key (camelCase) differs from the snake_case Rust field name,
   meaning a #[serde(rename = ...)] attribute is needed."
  (string/= (camel-to-snake key) key))

(defun rust-type-for-primitive (prim)
  "Map a primitive keyword to its Rust type string.
   :string -> \"String\", :int -> \"i64\", etc."
  (ecase prim
    (:string "String")
    (:int    "i64")
    (:float  "f64")
    (:bool   "bool")
    (:any    "serde_json::Value")))

(defun type-ref-to-rust (ref)
  "Convert a type-ref struct to its Rust type string."
  (etypecase ref
    (primitive-ref (rust-type-for-primitive (primitive-ref-prim ref)))
    (list-ref (format nil "Vec<~A>" (type-ref-to-rust (list-ref-element ref))))
    (map-ref (format nil "HashMap<String, ~A>" (type-ref-to-rust (map-ref-value-type ref))))
    (named-ref (named-ref-name ref))))

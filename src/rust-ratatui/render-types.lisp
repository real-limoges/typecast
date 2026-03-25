(in-package #:typecast/rust-ratatui)

;;;; Pass 2a: Render Rust data types (struct / enum / tagged union)
;;;;
;;;; These functions produce strings of Rust source code matching
;;;; the output of the existing build.rs.

(defun render-data-type (type-def)
  "Dispatch to the appropriate renderer for a type definition.
   Returns a string of Rust source code."
  (etypecase type-def
    (object-type (render-object-type type-def))
    (enum-type   (render-enum-type type-def))
    (union-type  (render-union-type type-def))))

;;; Object types -> Rust structs

(defun render-object-type (obj)
  "Render an object type as a Rust struct with serde derives."
  (with-output-to-string (out)
    (format out "#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]~%")
    (format out "pub struct ~A {~%" (object-type-name obj))
    (dolist (field (object-type-fields obj))
      (write-string (render-field field "    " t) out))
    (format out "}~%")))

(defun render-field (field indent pub-p)
  "Render a single struct field with serde attributes.
   INDENT is the indentation prefix.  PUB-P controls pub visibility."
  (let* ((key (field-def-key field))
         (snake (camel-to-snake key))
         (rust-name (rust-field-name snake))
         (rust-type (type-ref-to-rust (field-def-type-ref field)))
         (vis (if pub-p "pub " ""))
         (rename-needed (needs-rename-p key)))
    (with-output-to-string (out)
      (if (field-def-required field)
          ;; Required field
          (progn
            (when rename-needed
              (format out "~A#[serde(rename = ~S)]~%" indent key))
            (format out "~A~A~A: ~A,~%" indent vis rust-name rust-type))
          ;; Optional field
          (let ((rename-part (if rename-needed
                                 (format nil "rename = ~S, " key)
                                 "")))
            (format out "~A#[serde(~Askip_serializing_if = \"Option::is_none\")]~%"
                    indent rename-part)
            (format out "~A~A~A: Option<~A>,~%" indent vis rust-name rust-type))))))

;;; Enum types -> Rust enums

(defun render-enum-type (enum)
  "Render a simple enum type as a Rust enum with serde derives."
  (with-output-to-string (out)
    (format out "#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]~%")
    (format out "pub enum ~A {~%" (enum-type-name enum))
    (dolist (ev (enum-type-values enum))
      (let ((variant (to-pascal-case (enum-value-value ev))))
        (format out "    #[serde(rename = ~S)]~%" (enum-value-value ev))
        (format out "    ~A,~%" variant)))
    (format out "}~%")))

;;; Tagged union types -> Rust enums with variant payloads

(defun render-union-type (union)
  "Render a tagged union as a Rust enum with serde tag/untagged attributes."
  (with-output-to-string (out)
    (format out "#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]~%")
    (if (union-type-discriminator union)
        (format out "#[serde(tag = ~S)]~%" (union-type-discriminator union))
        (format out "#[serde(untagged)]~%"))
    (format out "pub enum ~A {~%" (union-type-name union))
    (dolist (variant (union-type-variants union))
      (let ((variant-name (to-pascal-case (variant-def-value variant))))
        (when (union-type-discriminator union)
          (format out "    #[serde(rename = ~S)]~%" (variant-def-value variant)))
        (if (null (variant-def-fields variant))
            ;; Unit variant
            (format out "    ~A,~%" variant-name)
            ;; Variant with fields
            (progn
              (format out "    ~A {~%" variant-name)
              (dolist (field (variant-def-fields variant))
                (write-string (render-field field "        " nil) out))
              (format out "    },~%")))))
    (format out "}~%")))

;;; Default impl rendering

(defun render-default-impl (type-def)
  "Render a Default impl for a type definition, if it has defaults.
   Returns a string or nil if no defaults are applicable."
  (etypecase type-def
    (enum-type
     (let* ((default-val (or (enum-type-default type-def)
                             (enum-value-value (first (enum-type-values type-def)))))
            (variant (to-pascal-case default-val))
            (name (enum-type-name type-def)))
       (with-output-to-string (out)
         (format out "impl Default for ~A {~%" name)
         (format out "    fn default() -> Self {~%")
         (format out "        ~A::~A~%" name variant)
         (format out "    }~%")
         (format out "}~%"))))
    (union-type
     (let* ((default-val (or (union-type-default-variant type-def)
                             (variant-def-value (first (union-type-variants type-def)))))
            (variant (to-pascal-case default-val))
            (name (union-type-name type-def))
            (vdef (find default-val (union-type-variants type-def)
                        :key #'variant-def-value :test #'string=)))
       (with-output-to-string (out)
         (format out "impl Default for ~A {~%" name)
         (format out "    fn default() -> Self {~%")
         (if (or (null vdef) (null (variant-def-fields vdef)))
             (format out "        ~A::~A~%" name variant)
             (progn
               (format out "        ~A::~A {~%" name variant)
               (dolist (field (variant-def-fields vdef))
                 (let* ((key (field-def-key field))
                        (snake (camel-to-snake key))
                        (rust-name (rust-field-name snake)))
                   (format out "            ~A: Default::default(),~%" rust-name)))
               (format out "        }~%")))
         (format out "    }~%")
         (format out "}~%"))))
    (object-type nil)))  ; object defaults are handled in FormComponent::from_model

(in-package #:typecast/core)

;;;; Schema — top-level container

(defstruct schema
  "Top-level schema parsed from JSON."
  (version 1 :type integer)
  (root "" :type string)
  (types (make-hash-table :test #'equal) :type hash-table))  ; string -> type-def

;;;; Type definitions (discriminated by :kind slot)

(defstruct type-def
  "Base type for all schema type definitions."
  (name "" :type string)
  (kind nil))  ; :object, :enum, or :union

(defstruct (object-type (:include type-def (kind :object)))
  "A struct with named fields."
  (fields nil :type list))  ; list of field-def

(defstruct (enum-type (:include type-def (kind :enum)))
  "A simple string enumeration."
  (values nil :type list)   ; list of enum-value
  (default nil))            ; string or nil

(defstruct (union-type (:include type-def (kind :union)))
  "A tagged union (sum type) with payload-carrying variants."
  (discriminator nil)       ; string (e.g. "mode") or nil (untagged)
  (default-variant nil)     ; string or nil
  (variants nil :type list))  ; list of variant-def

;;;; Field definitions

(defstruct field-def
  "A single field within an object or variant."
  (key "" :type string)       ; camelCase key from JSON
  (label nil)                 ; display label (string or nil)
  (help nil)                  ; help text (string or nil)
  (placeholder nil)           ; placeholder text (string or nil)
  (type-ref nil)              ; type-ref struct
  (required nil :type boolean)
  (default nil)               ; default value (any JSON value or nil)
  (constraints nil))          ; plist, e.g. (:min 1 :max 599 :min-items 1)

;;;; Type references

(defstruct type-ref
  "Base type for type references (the 'type' field in field-def)."
  (kind nil))  ; :primitive, :list, :map, or :named

(defstruct (primitive-ref (:include type-ref (kind :primitive)))
  "A primitive scalar type."
  (prim nil))  ; :string, :int, :float, :bool, :any

(defstruct (list-ref (:include type-ref (kind :list)))
  "A list type [T]."
  (element nil))  ; type-ref

(defstruct (map-ref (:include type-ref (kind :map)))
  "A map type map<string,V>."
  (value-type nil))  ; type-ref

(defstruct (named-ref (:include type-ref (kind :named)))
  "A reference to a named type defined in the schema."
  (name "" :type string))

;;;; Enum and union pieces

(defstruct enum-value
  "One variant of a simple enum."
  (value "" :type string)   ; wire value
  (label "" :type string))  ; display label

(defstruct variant-def
  "One variant of a tagged union."
  (value "" :type string)     ; wire value / discriminator value
  (label "" :type string)     ; display label
  (help nil)                  ; help text (string or nil)
  (fields nil :type list))    ; list of field-def (empty for unit variants)

;;;; Print methods for REPL inspection

(defmethod print-object ((obj schema) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "v~D root=~A types=~D"
            (schema-version obj)
            (schema-root obj)
            (hash-table-count (schema-types obj)))))

(defmethod print-object ((obj type-def) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A (~A)" (type-def-name obj) (type-def-kind obj))))

(defmethod print-object ((obj field-def) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A~A"
            (field-def-key obj)
            (type-ref-summary (field-def-type-ref obj))
            (if (field-def-required obj) "" " optional"))))

(defmethod print-object ((obj type-ref) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (type-ref-summary obj))))

(defgeneric type-ref-summary (ref)
  (:documentation "Short string description of a type-ref for debugging.")
  (:method ((ref primitive-ref))
    (string-downcase (symbol-name (primitive-ref-prim ref))))
  (:method ((ref list-ref))
    (format nil "[~A]" (type-ref-summary (list-ref-element ref))))
  (:method ((ref map-ref))
    (format nil "map<string,~A>" (type-ref-summary (map-ref-value-type ref))))
  (:method ((ref named-ref))
    (named-ref-name ref))
  (:method ((ref type-ref))
    "?"))

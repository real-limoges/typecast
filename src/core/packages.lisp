(defpackage #:typecast/core
  (:use #:cl)
  (:export
   ;; Utilities
   #:camel-to-snake

   ;; AST — schema
   #:schema
   #:make-schema
   #:schema-version
   #:schema-root
   #:schema-types
   #:schema-p

   ;; AST — type definitions
   #:type-def
   #:type-def-name
   #:type-def-kind
   #:type-def-p

   #:object-type
   #:make-object-type
   #:object-type-fields
   #:object-type-p

   #:enum-type
   #:make-enum-type
   #:enum-type-values
   #:enum-type-default
   #:enum-type-p

   #:union-type
   #:make-union-type
   #:union-type-discriminator
   #:union-type-default-variant
   #:union-type-variants
   #:union-type-p

   ;; AST — field definitions
   #:field-def
   #:make-field-def
   #:field-def-key
   #:field-def-label
   #:field-def-help
   #:field-def-placeholder
   #:field-def-type-ref
   #:field-def-required
   #:field-def-default
   #:field-def-constraints
   #:field-def-p

   ;; AST — type references
   #:type-ref
   #:type-ref-kind
   #:type-ref-summary
   #:type-ref-p

   #:primitive-ref
   #:make-primitive-ref
   #:primitive-ref-prim
   #:primitive-ref-p

   #:list-ref
   #:make-list-ref
   #:list-ref-element
   #:list-ref-p

   #:map-ref
   #:make-map-ref
   #:map-ref-value-type
   #:map-ref-p

   #:named-ref
   #:make-named-ref
   #:named-ref-name
   #:named-ref-p

   ;; AST — enum/union pieces
   #:enum-value
   #:make-enum-value
   #:enum-value-value
   #:enum-value-label

   #:variant-def
   #:make-variant-def
   #:variant-def-value
   #:variant-def-label
   #:variant-def-help
   #:variant-def-fields

   ;; Parse
   #:parse-schema

   ;; Validation & sorting
   #:resolve-named-refs
   #:topological-sort-types
   #:type-dependencies
   #:collect-named-refs))

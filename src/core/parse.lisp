(in-package #:typecast/core)

;;;; Pass 1: JSON -> AST
;;;;
;;;; Entry point: (parse-schema pathname) -> schema struct

(defun parse-schema (pathname)
  "Read a JSON schema file and return a SCHEMA struct."
  (let* ((json (with-open-file (in pathname :direction :input)
                 (yason:parse in :object-as :hash-table
                                 :object-key-fn #'identity)))
         (version (gethash "version" json 1))
         (root (gethash "root" json ""))
         (types-json (gethash "types" json (make-hash-table :test #'equal)))
         (types (make-hash-table :test #'equal)))
    (maphash (lambda (name def-json)
               (setf (gethash name types) (parse-type-def name def-json)))
             types-json)
    (make-schema :version version :root root :types types)))

;;; Type definition dispatching

(defun parse-type-def (name json)
  "Parse a single type definition from its JSON hash-table."
  (let ((kind (gethash "kind" json)))
    (cond
      ((string= kind "object")
       (make-object-type
        :name name
        :fields (mapcar #'parse-field-def
                        (gethash "fields" json nil))))
      ((string= kind "enum")
       (make-enum-type
        :name name
        :values (mapcar #'parse-enum-value
                        (gethash "values" json nil))
        :default (gethash "default" json nil)))
      ((string= kind "tagged_union")
       (make-union-type
        :name name
        :discriminator (gethash "discriminator" json nil)
        :default-variant (gethash "default_variant" json nil)
        :variants (mapcar #'parse-variant-def
                          (gethash "variants" json nil))))
      (t (error "Unknown type kind ~S for type ~A" kind name)))))

;;; Field definitions

(defun parse-field-def (json)
  "Parse a field definition from its JSON hash-table."
  (make-field-def
   :key (gethash "key" json "")
   :label (gethash "label" json nil)
   :help (gethash "help" json nil)
   :placeholder (gethash "placeholder" json nil)
   :type-ref (parse-type-expr (gethash "type" json ""))
   :required (eq (gethash "required" json nil) t)
   :default (gethash "default" json nil)
   :constraints (parse-constraints (gethash "constraints" json nil))))

(defun parse-constraints (json)
  "Parse a constraints JSON object into a plist.
   {\"min\": 1, \"max\": 599} -> (:min 1 :max 599)."
  (when json
    (let ((result nil))
      (maphash (lambda (k v)
                 (let* ((snake (camel-to-snake k))
                        (lisp-name (substitute #\- #\_ (string-upcase snake)))
                        (key (intern lisp-name :keyword)))
                   (push v result)
                   (push key result)))
               json)
      result)))

;;; Enum and union pieces

(defun parse-enum-value (json)
  "Parse an enum value {\"value\": ..., \"label\": ...}."
  (make-enum-value
   :value (gethash "value" json "")
   :label (gethash "label" json "")))

(defun parse-variant-def (json)
  "Parse a tagged union variant definition."
  (make-variant-def
   :value (gethash "value" json "")
   :label (gethash "label" json "")
   :help (gethash "help" json nil)
   :fields (mapcar #'parse-field-def
                   (gethash "fields" json nil))))

;;; Type expression parser

(defun parse-type-expr (s)
  "Parse a type expression string into a type-ref struct.
   \"[Foo]\"           -> list-ref wrapping named-ref
   \"map<string,Foo>\" -> map-ref
   \"string\"          -> primitive-ref :string
   \"Foo\"             -> named-ref \"Foo\""
  (cond
    ;; List: [T]
    ((and (> (length s) 2)
          (char= (char s 0) #\[)
          (char= (char s (1- (length s))) #\]))
     (make-list-ref :element (parse-type-expr (subseq s 1 (1- (length s))))))

    ;; Map: map<K,V>  (K is always string in this schema)
    ((and (> (length s) 4)
          (string= (subseq s 0 4) "map<")
          (char= (char s (1- (length s))) #\>))
     (let* ((inner (subseq s 4 (1- (length s))))
            (comma-pos (position #\, inner)))
       (unless comma-pos
         (error "Malformed map type expression: ~A" s))
       (make-map-ref
        :value-type (parse-type-expr (string-trim " " (subseq inner (1+ comma-pos)))))))

    ;; Primitives
    ((string= s "string") (make-primitive-ref :prim :string))
    ((string= s "int")    (make-primitive-ref :prim :int))
    ((string= s "float")  (make-primitive-ref :prim :float))
    ((string= s "bool")   (make-primitive-ref :prim :bool))
    ((string= s "any")    (make-primitive-ref :prim :any))

    ;; Named reference (everything else)
    (t (make-named-ref :name s))))

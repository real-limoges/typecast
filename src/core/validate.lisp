(in-package #:typecast/core)

;;;; Named-ref resolution and topological sort

(defun resolve-named-refs (schema)
  "Walk all type-refs in the schema and verify every named-ref points to
   a type that exists in the schema's type table.  Signals an error listing
   all unresolved references."
  (let ((types (schema-types schema))
        (errors nil))
    (maphash (lambda (name type-def)
               (dolist (ref (collect-named-refs type-def))
                 (unless (gethash (named-ref-name ref) types)
                   (push (format nil "~A: unresolved reference to ~A"
                                 name (named-ref-name ref))
                         errors))))
             types)
    (when errors
      (error "Unresolved named references:~%~{  ~A~%~}" (nreverse errors))))
  schema)

(defun collect-named-refs (type-def)
  "Collect all named-ref structs from a type definition's fields/variants."
  (let ((refs nil))
    (labels ((walk-ref (ref)
               (etypecase ref
                 (primitive-ref)
                 (list-ref (walk-ref (list-ref-element ref)))
                 (map-ref (walk-ref (map-ref-value-type ref)))
                 (named-ref (push ref refs))))
             (walk-field (f)
               (walk-ref (field-def-type-ref f))))
      (etypecase type-def
        (object-type
         (mapc #'walk-field (object-type-fields type-def)))
        (enum-type)  ; no fields to walk
        (union-type
         (dolist (v (union-type-variants type-def))
           (mapc #'walk-field (variant-def-fields v))))))
    refs))

;;; Topological sort (Kahn's algorithm)

(defun type-dependencies (type-def)
  "Return a list of type name strings that this type-def directly depends on."
  (remove-duplicates
   (mapcar #'named-ref-name (collect-named-refs type-def))
   :test #'string=))

(defun topological-sort-types (schema)
  "Return the schema's types as a list of (name . type-def) pairs in
   dependency order (leaves first).  Signals an error on cycles."
  (let* ((types (schema-types schema))
         ;; Build adjacency: for each type, which types depend on it?
         ;; And in-degree: how many dependencies does each type have?
         (in-degree (make-hash-table :test #'equal))
         (dependents (make-hash-table :test #'equal))  ; dep -> list of types that need dep
         (all-names nil))
    ;; Initialize
    (maphash (lambda (name td)
               (declare (ignore td))
               (push name all-names)
               (setf (gethash name in-degree) 0))
             types)
    ;; Build graph
    (maphash (lambda (name td)
               (let ((deps (type-dependencies td)))
                 (setf (gethash name in-degree) (length deps))
                 (dolist (dep deps)
                   (push name (gethash dep dependents nil)))))
             types)
    ;; Kahn's algorithm
    (let ((queue (loop for name in all-names
                       when (zerop (gethash name in-degree))
                         collect name))
          (result nil))
      ;; Sort the initial queue for deterministic output
      (setf queue (sort queue #'string<))
      (loop while queue
            for current = (pop queue)
            do (push (cons current (gethash current types)) result)
               (dolist (dependent (gethash current dependents nil))
                 (decf (gethash dependent in-degree))
                 (when (zerop (gethash dependent in-degree))
                   ;; Insert in sorted position for determinism
                   (setf queue (merge 'list (list dependent) queue #'string<)))))
      ;; Check for cycles
      (when (/= (length result) (hash-table-count types))
        (let ((remaining (loop for name in all-names
                               unless (assoc name result :test #'string=)
                                 collect name)))
          (error "Cycle detected among types: ~{~A~^, ~}" remaining)))
      (nreverse result))))

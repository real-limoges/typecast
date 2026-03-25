(in-package #:typecast/rust-ratatui)

;;;; Pass 2c: Render per-type FormComponent implementations
;;;;
;;;; For each schema type, generates a Rust component struct + trait impl
;;;; that goes into the same file as the data type.

;;; ─── Top-level dispatcher ───────────────────────────────────────────

(defun render-component (type-def schema)
  "Render the FormComponent code for a type definition.
   Returns a string of Rust source code, or nil for enums (which use SelectList)."
  (etypecase type-def
    (object-type (render-object-component type-def schema))
    (enum-type   (render-enum-select-constructor type-def))
    (union-type  (render-union-component type-def schema))))

;;; ─── Enum types → SelectList factory functions ──────────────────────

(defun render-enum-select-constructor (enum)
  "Generate a factory function that creates a SelectList for an enum type."
  (let ((name (enum-type-name enum))
        (snake (camel-to-snake (enum-type-name enum)))
        (default (enum-type-default enum)))
    (with-output-to-string (out)
      (format out "pub fn ~A_select(label: impl Into<String>) -> super::primitives::SelectList<~A> {~%"
              snake name)
      (format out "    super::primitives::SelectList::new(~%")
      (format out "        label,~%")
      (format out "        vec![~%")
      (dolist (ev (enum-type-values enum))
        (format out "            (~A::~A, ~S.into()),~%"
                name (to-pascal-case (enum-value-value ev)) (enum-value-label ev)))
      (format out "        ],~%")
      (if default
          (format out "        &~A::~A,~%" name (to-pascal-case default))
          (let ((first (first (enum-type-values enum))))
            (format out "        &~A::~A,~%" name (to-pascal-case (enum-value-value first)))))
      (format out "    )~%")
      (format out "}~%"))))

;;; ─── Object types → Form component ─────────────────────────────────

(defun component-type-for-field (field-def schema)
  "Determine the Rust component type for a field's type-ref.
   Returns (component-type . needs-optional-wrap)."
  (let* ((ref (field-def-type-ref field-def))
         (required (field-def-required field-def))
         (base (component-type-for-ref ref schema)))
    (if required
        base
        (format nil "super::optional_field::OptionalField<~A>" base))))

(defun component-type-for-ref (ref schema)
  "Map a type-ref to its component type string."
  (etypecase ref
    (primitive-ref
     (ecase (primitive-ref-prim ref)
       (:string "super::primitives::TextInput")
       (:int    "super::primitives::NumericInput")
       (:float  "super::primitives::FloatInput")
       (:bool   "super::primitives::Toggle")
       (:any    "super::primitives::JsonValueEditor")))
    (list-ref
     (let ((elem-comp (component-type-for-ref (list-ref-element ref) schema)))
       (format nil "super::list_editor::ListEditor<~A>" elem-comp)))
    (map-ref
     (let ((val-comp (component-type-for-ref (map-ref-value-type ref) schema)))
       (format nil "super::kv_editor::KvEditor<~A>" val-comp)))
    (named-ref
     (let* ((name (named-ref-name ref))
            (td (gethash name (schema-types schema))))
       (etypecase td
         (enum-type
          (format nil "super::primitives::SelectList<~A>" name))
         (object-type
          (format nil "super::~A::~AForm" (type-name-to-filename name) name))
         (union-type
          (format nil "super::~A::~AForm" (type-name-to-filename name) name)))))))

(defun render-object-form-struct (name form-name fields schema)
  "Render the form struct definition and from-model constructor for an object type."
  (with-output-to-string (out)
    (format out "#[derive(Default)]~%")
    (format out "pub struct ~A {~%" form-name)
    (dolist (f fields)
      (format out "    pub ~A: ~A,~%" (field-rust-name f) (component-type-for-field f schema)))
    (format out "}~%~%")
    (format out "impl ~A {~%" form-name)
    (format out "    pub fn new(model: &~A) -> Self {~%" name)
    (format out "        Self {~%")
    (dolist (f fields)
      (write-string (render-field-init f (field-rust-name f) schema "            ") out))
    (format out "        }~%")
    (format out "    }~%")
    (format out "}~%~%")))

(defun find-summary-field (fields)
  "Find the best field to use for summary display.
   Prefers 'name', then 'cmd', then 'url', then first required string field."
  (or (find "name" fields :key #'field-def-key :test #'string=)
      (find "cmd" fields :key #'field-def-key :test #'string=)
      (find "url" fields :key #'field-def-key :test #'string=)
      (find-if (lambda (f)
                 (and (field-def-required f)
                      (primitive-ref-p (field-def-type-ref f))
                      (eq (primitive-ref-prim (field-def-type-ref f)) :string)))
               fields)))

;;; ─── Object trait methods (extracted for clarity) ───────────────────

(defun render-object-to-model (name fields schema)
  "Render the to_model method for an object FormComponent impl."
  (with-output-to-string (out)
    (format out "    fn to_model(&self) -> ~A {~%" name)
    (format out "        ~A {~%" name)
    (dolist (f fields)
      (write-string (render-field-extract f (field-rust-name f) schema "            ") out))
    (format out "        }~%")
    (format out "    }~%~%")))

(defun render-object-validate (fields)
  "Render the validate method for an object FormComponent impl."
  (with-output-to-string (out)
    (format out "    fn validate(&mut self) -> Vec<String> {~%")
    (format out "        let mut errors = Vec::new();~%")
    (dolist (f fields)
      (format out "        errors.extend(self.~A.validate~A());~%"
              (field-rust-name f) (if (field-def-required f) "" "_impl")))
    (format out "        errors~%")
    (format out "    }~%~%")))

(defun render-object-field-count (fields)
  "Render the field_count method for an object FormComponent impl."
  (with-output-to-string (out)
    (format out "    fn field_count(&self) -> usize {~%")
    (if (null fields)
        (format out "        0~%")
        (progn
          (format out "        ")
          (loop for f in fields
                for i from 0
                for rname = (field-rust-name f)
                do (when (> i 0) (format out " + "))
                   (format out "self.~A.field_count~A()" rname (if (field-def-required f) "" "_impl")))
          (format out "~%")))
    (format out "    }~%~%")))

(defun render-object-handle-key (fields)
  "Render the handle_key method for an object FormComponent impl."
  (with-output-to-string (out)
    (format out "    fn handle_key(&mut self, focus_offset: usize, key: crossterm::event::KeyEvent) -> super::traits::EventResult {~%")
    (format out "        let mut remaining = focus_offset;~%")
    (dolist (f fields)
      (let ((rname (field-rust-name f))
            (impl (if (field-def-required f) "" "_impl")))
        (format out "        let count = self.~A.field_count~A();~%" rname impl)
        (format out "        if remaining < count {~%")
        (format out "            return self.~A.handle_key~A(remaining, key);~%" rname impl)
        (format out "        }~%")
        (format out "        remaining -= count;~%")))
    (format out "        let _ = remaining;~%")
    (format out "        super::traits::EventResult::Ignored~%")
    (format out "    }~%~%")))

(defun render-object-render-lines (fields)
  "Render the render_lines method for an object FormComponent impl."
  (with-output-to-string (out)
    (format out "    fn render_lines(~%")
    (format out "        &self,~%")
    (format out "        focused_field: Option<usize>,~%")
    (format out "        show_errors: bool,~%")
    (format out "    ) -> Vec<ratatui::text::Line<'static>> {~%")
    (format out "        let mut lines = Vec::new();~%")
    (format out "        let mut offset = 0usize;~%")
    (dolist (f fields)
      (let ((rname (field-rust-name f))
            (impl (if (field-def-required f) "" "_impl")))
        (format out "        {~%")
        (format out "            let count = self.~A.field_count~A();~%" rname impl)
        (format out "            let local_focus = focused_field.and_then(|f| {~%")
        (format out "                if f >= offset && f < offset + count { Some(f - offset) } else { None }~%")
        (format out "            });~%")
        (format out "            lines.extend(self.~A.render_lines~A(local_focus, show_errors));~%" rname impl)
        (format out "            offset += count;~%")
        (format out "        }~%")))
    (format out "        let _ = offset;~%")
    (format out "        lines~%")
    (format out "    }~%~%")))

(defun render-object-trait-impl (name form-name fields schema)
  "Render the FormComponent trait impl block for an object form."
  (with-output-to-string (out)
    (format out "impl super::traits::FormComponent for ~A {~%" form-name)
    (format out "    type Model = ~A;~%~%" name)
    (format out "    fn from_model(model: &~A) -> Self {~%" name)
    (format out "        Self::new(model)~%")
    (format out "    }~%~%")
    (write-string (render-object-to-model name fields schema) out)
    (write-string (render-object-validate fields) out)
    (write-string (render-object-field-count fields) out)
    (write-string (render-object-handle-key fields) out)
    (write-string (render-object-render-lines fields) out)
    (let ((summary-field (find-summary-field fields)))
      (format out "    fn summary_line(&self) -> String {~%")
      (if summary-field
          (format out "        self.~A.summary_line()~%" (field-rust-name summary-field))
          (format out "        ~S.into()~%" name))
      (format out "    }~%"))
    (format out "}~%")))

(defun render-object-component (obj schema)
  "Generate the Form component struct and FormComponent impl for an object type."
  (let* ((name (object-type-name obj))
         (form-name (format nil "~AForm" name))
         (fields (object-type-fields obj)))
    (concatenate 'string
      (render-object-form-struct name form-name fields schema)
      (render-object-trait-impl name form-name fields schema))))

;;; ─── Shared constraint and metadata helpers ─────────────────────────

(defun emit-int-constraints (out indent constraints)
  "Emit w.min/w.max assignments for integer constraints."
  (when (getf constraints :min)
    (format out "~A    w.min = Some(~A);~%" indent (getf constraints :min)))
  (when (getf constraints :max)
    (format out "~A    w.max = Some(~A);~%" indent (getf constraints :max))))

(defun emit-float-constraints (out indent constraints)
  "Emit w.min/w.max/w.min_exclusive assignments for float constraints."
  (when (getf constraints :min)
    (format out "~A    w.min = Some(~A as f64);~%" indent (getf constraints :min)))
  (when (getf constraints :max)
    (format out "~A    w.max = Some(~A as f64);~%" indent (getf constraints :max)))
  (when (getf constraints :min-exclusive)
    (format out "~A    w.min_exclusive = Some(~A as f64);~%" indent (getf constraints :min-exclusive))))

(defun emit-field-metadata (out indent help placeholder)
  "Emit w.placeholder and w.help assignments when present."
  (when placeholder
    (format out "~A    w.placeholder = Some(~S.into());~%" indent placeholder))
  (when help
    (format out "~A    w.help = Some(~S.into());~%" indent help)))

;;; ─── Field initialization (from_model) ──────────────────────────────

(defun render-field-init (field rname schema indent)
  "Render the initialization expression for a field in from_model."
  (let ((ref (field-def-type-ref field))
        (label (or (field-def-label field) (field-def-key field)))
        (help (field-def-help field))
        (placeholder (field-def-placeholder field))
        (constraints (field-def-constraints field))
        (required (field-def-required field)))
    (with-output-to-string (out)
      (if required
          (write-string (render-required-field-init ref rname label help placeholder constraints schema indent) out)
          (write-string (render-optional-field-init ref rname label help placeholder constraints schema indent) out)))))

(defun render-primitive-field-init (prim rname src src-ref src-deref label help placeholder constraints indent)
  "Render initialization for a required primitive field widget.
   SRC/SRC-REF/SRC-DEREF are model access expressions whose form depends on context."
  (with-output-to-string (out)
    (ecase prim
      (:string
       (format out "~A~A: {~%" indent rname)
       (format out "~A    let mut w = super::primitives::TextInput::new(~S, true);~%" indent label)
       (format out "~A    w.input = tui_input::Input::new(~A.clone());~%" indent src)
       (emit-field-metadata out indent help placeholder)
       (format out "~A    w~%" indent)
       (format out "~A},~%" indent))
      (:int
       (format out "~A~A: {~%" indent rname)
       (format out "~A    let mut w = super::primitives::NumericInput::new(~S, true);~%" indent label)
       (format out "~A    w.input = tui_input::Input::new(~A.to_string());~%" indent src)
       (emit-int-constraints out indent constraints)
       (emit-field-metadata out indent help placeholder)
       (format out "~A    w~%" indent)
       (format out "~A},~%" indent))
      (:float
       (format out "~A~A: {~%" indent rname)
       (format out "~A    let mut w = super::primitives::FloatInput::new(~S, true);~%" indent label)
       (format out "~A    w.input = tui_input::Input::new(~A.to_string());~%" indent src)
       (emit-float-constraints out indent constraints)
       (emit-field-metadata out indent help placeholder)
       (format out "~A    w~%" indent)
       (format out "~A},~%" indent))
      (:bool
       (format out "~A~A: super::primitives::Toggle::new(~S, ~A),~%"
               indent rname label src-deref))
      (:any
       (format out "~A~A: {~%" indent rname)
       (format out "~A    let mut w = super::primitives::JsonValueEditor::from_model(~A);~%" indent src-ref)
       (format out "~A    w.label = ~S.into();~%" indent label)
       (when help
         (format out "~A    w.help = Some(~S.into());~%" indent help))
       (format out "~A    w~%" indent)
       (format out "~A},~%" indent)))))

(defun render-required-field-init (ref rname label help placeholder constraints schema indent &key use-local-p)
  "Render initialization for a required field.
   When USE-LOCAL-P is true, reads from a destructured local variable (union match arm)
   rather than model.FIELD."
  (let* ((src (if use-local-p rname (format nil "model.~A" rname)))
         (src-ref (if use-local-p rname (format nil "&model.~A" rname)))
         (src-deref (if use-local-p (format nil "*~A" rname) (format nil "model.~A" rname))))
    (with-output-to-string (out)
      (etypecase ref
        (primitive-ref
         (write-string (render-primitive-field-init
                        (primitive-ref-prim ref) rname src src-ref src-deref
                        label help placeholder constraints indent) out))

        (named-ref
         (let* ((tname (named-ref-name ref))
                (td (gethash tname (schema-types schema))))
           (etypecase td
             (enum-type
              (format out "~A~A: super::~A::~A_select(~S),~%"
                      indent rname (type-name-to-filename tname) (camel-to-snake tname) label))
             (object-type
              (format out "~A~A: super::~A::~AForm::new(~A),~%"
                      indent rname (type-name-to-filename tname) tname src-ref))
             (union-type
              (format out "~A~A: super::~A::~AForm::new(~A),~%"
                      indent rname (type-name-to-filename tname) tname src-ref)))))

        (list-ref
         (let* ((min-items (or (getf constraints :min-items) 0)))
           (format out "~A~A: super::list_editor::ListEditor::from_models(~S, ~A, ~D),~%"
                   indent rname label src-ref min-items)))

        (map-ref
         (format out "~A~A: {~%" indent rname)
         (format out "~A    let pairs: Vec<(String, _)> = ~A.iter()~%" indent src)
         (format out "~A        .map(|(k, v)| (k.clone(), v.clone()))~%" indent)
         (format out "~A        .collect();~%" indent)
         (format out "~A    super::kv_editor::KvEditor::from_pairs(~S, &pairs)~%" indent label)
         (format out "~A},~%" indent))))))

;;; ─── Optional field initialization ───────────────────────────────────

(defun render-optional-primitive-init (prim rname label help placeholder constraints indent)
  "Render initialization for an optional primitive field, preserving metadata."
  (with-output-to-string (out)
    (ecase prim
      (:string
       (format out "~A~A: {~%" indent rname)
       (format out "~A    let mut w = super::primitives::TextInput::new(~S, true);~%" indent label)
       (emit-field-metadata out indent help placeholder)
       (format out "~A    if let Some(val) = &model.~A {~%" indent rname)
       (format out "~A        w.input = tui_input::Input::new(val.clone());~%" indent)
       (format out "~A    }~%" indent)
       (format out "~A    super::optional_field::OptionalField { label: ~S.into(), enabled: model.~A.is_some(), inner: w }~%" indent label rname)
       (format out "~A},~%" indent))
      (:int
       (format out "~A~A: {~%" indent rname)
       (format out "~A    let mut w = super::primitives::NumericInput::new(~S, true);~%" indent label)
       (emit-int-constraints out indent constraints)
       (emit-field-metadata out indent help placeholder)
       (format out "~A    if let Some(val) = &model.~A {~%" indent rname)
       (format out "~A        w.input = tui_input::Input::new(val.to_string());~%" indent)
       (format out "~A    }~%" indent)
       (format out "~A    super::optional_field::OptionalField { label: ~S.into(), enabled: model.~A.is_some(), inner: w }~%" indent label rname)
       (format out "~A},~%" indent))
      (:float
       (format out "~A~A: {~%" indent rname)
       (format out "~A    let mut w = super::primitives::FloatInput::new(~S, true);~%" indent label)
       (emit-float-constraints out indent constraints)
       (emit-field-metadata out indent help placeholder)
       (format out "~A    if let Some(val) = &model.~A {~%" indent rname)
       (format out "~A        w.input = tui_input::Input::new(val.to_string());~%" indent)
       (format out "~A    }~%" indent)
       (format out "~A    super::optional_field::OptionalField { label: ~S.into(), enabled: model.~A.is_some(), inner: w }~%" indent label rname)
       (format out "~A},~%" indent))
      (:bool
       (format out "~A~A: {~%" indent rname)
       (format out "~A    let inner = super::primitives::Toggle::new(~S, model.~A.unwrap_or(false));~%" indent label rname)
       (format out "~A    super::optional_field::OptionalField { label: ~S.into(), enabled: model.~A.is_some(), inner }~%" indent label rname)
       (format out "~A},~%" indent))
      (:any
       (format out "~A~A: {~%" indent rname)
       (format out "~A    let mut w = match &model.~A {~%" indent rname)
       (format out "~A        Some(val) => super::primitives::JsonValueEditor::from_model(val),~%" indent)
       (format out "~A        None => super::primitives::JsonValueEditor::from_model(&serde_json::Value::default()),~%" indent)
       (format out "~A    };~%" indent)
       (format out "~A    w.label = ~S.into();~%" indent label)
       (when help
         (format out "~A    w.help = Some(~S.into());~%" indent help))
       (format out "~A    super::optional_field::OptionalField { label: ~S.into(), enabled: model.~A.is_some(), inner: w }~%" indent label rname)
       (format out "~A},~%" indent)))))

(defun render-optional-field-init (ref rname label help placeholder constraints schema indent)
  "Render initialization for an optional field, preserving metadata on inner widgets."
  (with-output-to-string (out)
    (etypecase ref
      (primitive-ref
       (write-string (render-optional-primitive-init
                      (primitive-ref-prim ref) rname label help placeholder constraints indent) out))
      (named-ref
       (let* ((tname (named-ref-name ref))
              (td (gethash tname (schema-types schema))))
         (etypecase td
           (enum-type
            (format out "~A~A: {~%" indent rname)
            (format out "~A    let mut inner = super::~A::~A_select(~S);~%"
                    indent (type-name-to-filename tname) (camel-to-snake tname) label)
            (format out "~A    if let Some(val) = &model.~A {~%" indent rname)
            (format out "~A        if let Some(idx) = inner.options.iter().position(|(v, _)| v == val) {~%" indent)
            (format out "~A            inner.selected = idx;~%" indent)
            (format out "~A        }~%" indent)
            (format out "~A    }~%" indent)
            (format out "~A    super::optional_field::OptionalField { label: ~S.into(), enabled: model.~A.is_some(), inner }~%"
                    indent label rname)
            (format out "~A},~%" indent))
           ((or object-type union-type)
            (format out "~A~A: {~%" indent rname)
            (format out "~A    let inner = match &model.~A {~%" indent rname)
            (format out "~A        Some(val) => super::~A::~AForm::new(val),~%"
                    indent (type-name-to-filename tname) tname)
            (format out "~A        None => super::~A::~AForm::default(),~%"
                    indent (type-name-to-filename tname) tname)
            (format out "~A    };~%" indent)
            (format out "~A    super::optional_field::OptionalField { label: ~S.into(), enabled: model.~A.is_some(), inner }~%"
                    indent label rname)
            (format out "~A},~%" indent)))))
      (list-ref
       (let ((min-items (or (getf constraints :min-items) 0)))
         (format out "~A~A: {~%" indent rname)
         (format out "~A    let inner = match &model.~A {~%" indent rname)
         (format out "~A        Some(items) => super::list_editor::ListEditor::from_models(~S, items, ~D),~%"
                 indent label min-items)
         (format out "~A        None => super::list_editor::ListEditor::new(~S, ~D),~%"
                 indent label min-items)
         (format out "~A    };~%" indent)
         (format out "~A    super::optional_field::OptionalField { label: ~S.into(), enabled: model.~A.is_some(), inner }~%"
                 indent label rname)
         (format out "~A},~%" indent)))
      (map-ref
       (format out "~A~A: {~%" indent rname)
       (format out "~A    let inner = match &model.~A {~%" indent rname)
       (format out "~A        Some(m) => {~%" indent)
       (format out "~A            let pairs: Vec<(String, _)> = m.iter()~%" indent)
       (format out "~A                .map(|(k, v)| (k.clone(), v.clone()))~%" indent)
       (format out "~A                .collect();~%" indent)
       (format out "~A            super::kv_editor::KvEditor::from_pairs(~S, &pairs)~%" indent label)
       (format out "~A        }~%" indent)
       (format out "~A        None => super::kv_editor::KvEditor::new(~S),~%" indent label)
       (format out "~A    };~%" indent)
       (format out "~A    super::optional_field::OptionalField { label: ~S.into(), enabled: model.~A.is_some(), inner }~%"
               indent label rname)
       (format out "~A},~%" indent)))))

;;; ─── Field extraction (to_model) ────────────────────────────────────

(defun render-field-extract (field rname schema indent)
  "Render the to_model extraction expression for a field."
  (let ((ref (field-def-type-ref field))
        (required (field-def-required field)))
    (with-output-to-string (out)
      (if (not required)
          ;; Optional field
          (format out "~A~A: self.~A.to_option(),~%" indent rname rname)
          ;; Required field
          (etypecase ref
            (primitive-ref
             (ecase (primitive-ref-prim ref)
               ((:string :int :float :bool :any)
                (format out "~A~A: self.~A.to_model(),~%" indent rname rname))))
            (named-ref
             (let ((td (gethash (named-ref-name ref) (schema-types schema))))
               (etypecase td
                 (enum-type
                  (format out "~A~A: self.~A.current_value().clone(),~%" indent rname rname))
                 ((or object-type union-type)
                  (format out "~A~A: self.~A.to_model(),~%" indent rname rname)))))
            (list-ref
             (format out "~A~A: self.~A.to_models(),~%" indent rname rname))
            (map-ref
             (format out "~A~A: self.~A.to_pairs().into_iter().collect(),~%" indent rname rname)))))))

;;; ─── Tagged union types → Form component ────────────────────────────

(defun variant-fields-suffix (variants)
  "Build \", foo_fields, bar_fields\" string for variants that carry fields."
  (with-output-to-string (s)
    (dolist (v variants)
      (when (variant-def-fields v)
        (format s ", ~A_fields" (camel-to-snake (variant-def-value v)))))))

(defun variant-default-fields-list (variants &optional current-variant)
  "Build the comma-separated default fields for a match arm.
   When CURRENT-VARIANT is given, use 'fields' for that variant instead of default."
  (with-output-to-string (s)
    (dolist (v variants)
      (when (variant-def-fields v)
        (if (and current-variant (string= (variant-def-value v) current-variant))
            (format s ", fields")
            (format s ", ~AFields::default()" (to-pascal-case (variant-def-value v))))))))

(defun render-variant-selector-enum (selector-enum variants default-variant)
  "Emit the FooVariant enum used as the union discriminator in the selector widget."
  (with-output-to-string (out)
    (format out "#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]~%")
    (format out "pub enum ~A {~%" selector-enum)
    (dolist (v variants)
      (when (string= (variant-def-value v) default-variant)
        (format out "    #[default]~%"))
      (format out "    ~A,~%" (to-pascal-case (variant-def-value v))))
    (format out "}~%~%")))

(defun render-variant-field-structs (variants schema)
  "Emit per-variant FooFields structs for variants that carry fields."
  (with-output-to-string (out)
    (dolist (v variants)
      (when (variant-def-fields v)
        (let ((fields-struct (format nil "~AFields" (to-pascal-case (variant-def-value v)))))
          (format out "pub struct ~A {~%" fields-struct)
          (dolist (f (variant-def-fields v))
            (format out "    pub ~A: ~A,~%" (field-rust-name f) (component-type-for-field f schema)))
          (format out "}~%~%"))))))

(defun render-union-form-struct (form-name selector-enum variants)
  "Emit the main FooForm struct definition."
  (with-output-to-string (out)
    (format out "#[derive(Default)]~%")
    (format out "pub struct ~A {~%" form-name)
    (format out "    pub variant: ~A,~%" selector-enum)
    (dolist (v variants)
      (when (variant-def-fields v)
        (format out "    pub ~A_fields: ~AFields,~%"
                (camel-to-snake (variant-def-value v))
                (to-pascal-case (variant-def-value v)))))
    (format out "    label: String,~%")
    (format out "}~%~%")))

;;; ─── Union impl methods (split from render-union-impl-block) ────────

(defun render-union-new-method (name selector-enum variants schema)
  "Render the new(model) constructor for a union form."
  (let ((fields-suffix (variant-fields-suffix variants)))
    (with-output-to-string (out)
      (format out "    pub fn new(model: &~A) -> Self {~%" name)
      (format out "        let (variant~A) = match model {~%" fields-suffix)
      (dolist (v variants)
        (let ((pascal (to-pascal-case (variant-def-value v))))
          (if (null (variant-def-fields v))
              (format out "            ~A::~A => (~A::~A~A),~%"
                      name pascal selector-enum pascal
                      (variant-default-fields-list variants))
              (progn
                (format out "            ~A::~A { ~{~A~^, ~} } => {~%"
                        name pascal
                        (mapcar #'field-rust-name (variant-def-fields v)))
                (format out "                let fields = ~AFields {~%" pascal)
                (dolist (f (variant-def-fields v))
                  (write-string (render-required-field-init
                                 (field-def-type-ref f) (field-rust-name f)
                                 (or (field-def-label f) (field-def-key f))
                                 (field-def-help f) (field-def-placeholder f)
                                 (field-def-constraints f) schema "                    "
                                 :use-local-p t)
                                out))
                (format out "                };~%")
                (format out "                (~A::~A~A)~%"
                        selector-enum pascal
                        (variant-default-fields-list variants (variant-def-value v)))
                (format out "            }~%")))))
      (format out "        };~%")
      (format out "        Self { variant~A, label: String::new() }~%" fields-suffix)
      (format out "    }~%~%"))))

(defun render-union-variant-options-method (selector-enum variants)
  "Render the variant_options() method for a union form."
  (with-output-to-string (out)
    (format out "    fn variant_options() -> Vec<(~A, String)> {~%" selector-enum)
    (format out "        vec![~%")
    (dolist (v variants)
      (format out "            (~A::~A, ~S.into()),~%"
              selector-enum (to-pascal-case (variant-def-value v))
              (variant-def-label v)))
    (format out "        ]~%")
    (format out "    }~%~%")))

(defun render-union-active-field-count-method (selector-enum variants)
  "Render the active_field_count() method for a union form."
  (with-output-to-string (out)
    (format out "    fn active_field_count(&self) -> usize {~%")
    (format out "        match self.variant {~%")
    (dolist (v variants)
      (let ((pascal (to-pascal-case (variant-def-value v)))
            (snake (camel-to-snake (variant-def-value v))))
        (if (null (variant-def-fields v))
            (format out "            ~A::~A => 0,~%" selector-enum pascal)
            (progn
              (format out "            ~A::~A => ~%" selector-enum pascal)
              (loop for f in (variant-def-fields v)
                    for i from 0
                    for rname = (field-rust-name f)
                    do (when (> i 0) (format out " + "))
                       (format out "self.~A_fields.~A.field_count~A()"
                               snake rname (if (field-def-required f) "" "_impl")))
              (format out ",~%")))))
    (format out "        }~%")
    (format out "    }~%")))

(defun render-union-impl-block (name form-name selector-enum variants schema)
  "Emit impl FooForm { new, variant_options, active_field_count }."
  (with-output-to-string (out)
    (format out "impl ~A {~%" form-name)
    (write-string (render-union-new-method name selector-enum variants schema) out)
    (write-string (render-union-variant-options-method selector-enum variants) out)
    (write-string (render-union-active-field-count-method selector-enum variants) out)
    (format out "}~%~%")))

;;; ─── Union Default impls ────────────────────────────────────────────

(defun render-union-default-impls (variants schema)
  "Emit Default impls for per-variant FooFields structs."
  (with-output-to-string (out)
    (dolist (v variants)
      (when (variant-def-fields v)
        (let ((fields-struct (format nil "~AFields" (to-pascal-case (variant-def-value v)))))
          (format out "impl Default for ~A {~%" fields-struct)
          (format out "    fn default() -> Self {~%")
          (format out "        Self {~%")
          (dolist (f (variant-def-fields v))
            (write-string (render-default-field-init f (field-rust-name f) schema "            ") out))
          (format out "        }~%")
          (format out "    }~%")
          (format out "}~%~%"))))))

;;; ─── Union trait methods ────────────────────────────────────────────

(defun render-union-to-model (name selector-enum variants)
  "Render the to_model method for a union FormComponent impl."
  (with-output-to-string (out)
    (format out "    fn to_model(&self) -> ~A {~%" name)
    (format out "        match self.variant {~%")
    (dolist (v variants)
      (let ((pascal (to-pascal-case (variant-def-value v)))
            (snake (camel-to-snake (variant-def-value v))))
        (if (null (variant-def-fields v))
            (format out "            ~A::~A => ~A::~A,~%" selector-enum pascal name pascal)
            (progn
              (format out "            ~A::~A => ~A::~A {~%" selector-enum pascal name pascal)
              (dolist (f (variant-def-fields v))
                (let ((rname (field-rust-name f)))
                  (format out "                ~A: self.~A_fields.~A.to_model(),~%" rname snake rname)))
              (format out "            },~%")))))
    (format out "        }~%")
    (format out "    }~%~%")))

(defun render-union-validate (selector-enum variants)
  "Render the validate method for a union FormComponent impl."
  (with-output-to-string (out)
    (format out "    fn validate(&mut self) -> Vec<String> {~%")
    (format out "        match self.variant {~%")
    (dolist (v variants)
      (let ((pascal (to-pascal-case (variant-def-value v)))
            (snake (camel-to-snake (variant-def-value v))))
        (if (null (variant-def-fields v))
            (format out "            ~A::~A => vec![],~%" selector-enum pascal)
            (progn
              (format out "            ~A::~A => {~%" selector-enum pascal)
              (format out "                let mut errors = Vec::new();~%")
              (dolist (f (variant-def-fields v))
                (let ((rname (field-rust-name f)))
                  (format out "                errors.extend(self.~A_fields.~A.validate~A());~%"
                          snake rname (if (field-def-required f) "" "_impl"))))
              (format out "                errors~%")
              (format out "            }~%")))))
    (format out "        }~%")
    (format out "    }~%~%")))

(defun render-union-handle-key (selector-enum variants)
  "Render the handle_key method for a union FormComponent impl.
   Focus offset 0 drives the variant selector; higher offsets dispatch to active variant fields."
  (with-output-to-string (out)
    (format out "    fn handle_key(&mut self, focus_offset: usize, key: crossterm::event::KeyEvent) -> super::traits::EventResult {~%")
    (format out "        if focus_offset == 0 {~%")
    (format out "            match key.code {~%")
    (format out "                crossterm::event::KeyCode::Right | crossterm::event::KeyCode::Char(' ') | crossterm::event::KeyCode::Enter => {~%")
    (format out "                    let variants = Self::variant_options();~%")
    (format out "                    let idx = variants.iter().position(|(v, _)| *v == self.variant).unwrap_or(0);~%")
    (format out "                    self.variant = variants[(idx + 1) % variants.len()].0;~%")
    (format out "                    super::traits::EventResult::Consumed~%")
    (format out "                }~%")
    (format out "                crossterm::event::KeyCode::Left => {~%")
    (format out "                    let variants = Self::variant_options();~%")
    (format out "                    let idx = variants.iter().position(|(v, _)| *v == self.variant).unwrap_or(0);~%")
    (format out "                    self.variant = variants[(idx + variants.len() - 1) % variants.len()].0;~%")
    (format out "                    super::traits::EventResult::Consumed~%")
    (format out "                }~%")
    (format out "                _ => super::traits::EventResult::Ignored,~%")
    (format out "            }~%")
    (format out "        } else {~%")
    (format out "            let inner_offset = focus_offset - 1;~%")
    (format out "            match self.variant {~%")
    (dolist (v variants)
      (let ((pascal (to-pascal-case (variant-def-value v)))
            (snake (camel-to-snake (variant-def-value v))))
        (if (null (variant-def-fields v))
            (format out "                ~A::~A => super::traits::EventResult::Ignored,~%" selector-enum pascal)
            (progn
              (format out "                ~A::~A => {~%" selector-enum pascal)
              (format out "                    let mut remaining = inner_offset;~%")
              (dolist (f (variant-def-fields v))
                (let ((rname (field-rust-name f))
                      (impl (if (field-def-required f) "" "_impl")))
                  (format out "                    let count = self.~A_fields.~A.field_count~A();~%" snake rname impl)
                  (format out "                    if remaining < count { return self.~A_fields.~A.handle_key~A(remaining, key); }~%" snake rname impl)
                  (format out "                    remaining -= count;~%")))
              (format out "                    let _ = remaining;~%")
              (format out "                    super::traits::EventResult::Ignored~%")
              (format out "                }~%")))))
    (format out "            }~%")
    (format out "        }~%")
    (format out "    }~%~%")))

(defun render-union-render-lines (name selector-enum variants)
  "Render the render_lines method for a union FormComponent impl.
   Emits the header selector row, focused branch with offset tracking, and unfocused branch."
  (with-output-to-string (out)
    (format out "    fn render_lines(&self, focused_field: Option<usize>, show_errors: bool) -> Vec<ratatui::text::Line<'static>> {~%")
    (format out "        use ratatui::text::Span;~%")
    (format out "        use ratatui::style::{Color, Style};~%")
    (format out "        let mut lines = Vec::new();~%")
    (format out "        let selector_focused = focused_field == Some(0);~%")
    (format out "        let style = if selector_focused { Style::default().fg(Color::Black).bg(Color::Cyan) } else { Style::default().fg(Color::White) };~%")
    (format out "        let variant_label = match self.variant {~%")
    (dolist (v variants)
      (format out "            ~A::~A => ~S,~%"
              selector-enum (to-pascal-case (variant-def-value v)) (variant-def-label v)))
    (format out "        };~%")
    (format out "        lines.push(ratatui::text::Line::from(vec![~%")
    (format out "            Span::raw(format!(\"~A: \")),~%" name)
    (format out "            Span::styled(format!(\"[ < {variant_label} > ]\"), style),~%")
    (format out "        ]));~%")
    ;; Focused branch: track offset to compute per-field local focus
    (format out "        if let Some(focus) = focused_field {~%")
    (format out "            let inner_focus = if focus > 0 { Some(focus - 1) } else { None };~%")
    (format out "            match self.variant {~%")
    (dolist (v variants)
      (let ((pascal (to-pascal-case (variant-def-value v)))
            (snake (camel-to-snake (variant-def-value v))))
        (if (null (variant-def-fields v))
            (format out "                ~A::~A => {}~%" selector-enum pascal)
            (progn
              (format out "                ~A::~A => {~%" selector-enum pascal)
              (format out "                    let mut offset = 0usize;~%")
              (dolist (f (variant-def-fields v))
                (let ((rname (field-rust-name f))
                      (impl (if (field-def-required f) "" "_impl")))
                  (format out "                    let count = self.~A_fields.~A.field_count~A();~%" snake rname impl)
                  (format out "                    let lf = inner_focus.and_then(|f| if f >= offset && f < offset + count { Some(f - offset) } else { None });~%")
                  (format out "                    lines.extend(self.~A_fields.~A.render_lines~A(lf, show_errors));~%" snake rname impl)
                  (format out "                    offset += count;~%")))
              (format out "                    let _ = offset;~%")
              (format out "                }~%")))))
    (format out "            }~%")
    ;; Unfocused branch: pass None as focus to all fields
    (format out "        } else {~%")
    (format out "            match self.variant {~%")
    (dolist (v variants)
      (let ((pascal (to-pascal-case (variant-def-value v)))
            (snake (camel-to-snake (variant-def-value v))))
        (if (null (variant-def-fields v))
            (format out "                ~A::~A => {}~%" selector-enum pascal)
            (progn
              (format out "                ~A::~A => {~%" selector-enum pascal)
              (dolist (f (variant-def-fields v))
                (let ((rname (field-rust-name f))
                      (impl (if (field-def-required f) "" "_impl")))
                  (format out "                    lines.extend(self.~A_fields.~A.render_lines~A(None, show_errors));~%" snake rname impl)))
              (format out "                }~%")))))
    (format out "            }~%")
    (format out "        }~%")
    (format out "        lines~%")
    (format out "    }~%~%")))

(defun render-union-trait-impl (name form-name selector-enum variants)
  "Emit the FormComponent trait impl for a union form."
  (with-output-to-string (out)
    (format out "impl super::traits::FormComponent for ~A {~%" form-name)
    (format out "    type Model = ~A;~%~%" name)
    (format out "    fn from_model(model: &~A) -> Self { Self::new(model) }~%~%" name)
    (write-string (render-union-to-model name selector-enum variants) out)
    (write-string (render-union-validate selector-enum variants) out)
    (format out "    fn field_count(&self) -> usize { 1 + self.active_field_count() }~%~%")
    (write-string (render-union-handle-key selector-enum variants) out)
    (write-string (render-union-render-lines name selector-enum variants) out)
    ;; summary_line — small enough to stay inline
    (format out "    fn summary_line(&self) -> String {~%")
    (format out "        match self.variant {~%")
    (dolist (v variants)
      (format out "            ~A::~A => ~S.into(),~%"
              selector-enum (to-pascal-case (variant-def-value v)) (variant-def-label v)))
    (format out "        }~%")
    (format out "    }~%")
    (format out "}~%")))

(defun render-union-component (union schema)
  "Generate the Form component for a tagged union type.
   Orchestrates the sub-renderers into a complete Rust source section."
  (let* ((name (union-type-name union))
         (form-name (format nil "~AForm" name))
         (selector-enum (format nil "~AVariant" name))
         (variants (union-type-variants union))
         (default-variant (or (union-type-default-variant union)
                              (variant-def-value (first variants)))))
    (concatenate 'string
      (render-variant-selector-enum selector-enum variants default-variant)
      (render-variant-field-structs variants schema)
      (render-union-form-struct form-name selector-enum variants)
      (render-union-impl-block name form-name selector-enum variants schema)
      (render-union-default-impls variants schema)
      (render-union-trait-impl name form-name selector-enum variants))))

;;; ─── Default field initialization (for variant Default impls) ───────

(defun render-default-field-init (field rname schema indent)
  "Render a default initialization for a field (used in Default impls for variant structs)."
  (let ((ref (field-def-type-ref field))
        (default-val (field-def-default field))
        (label (or (field-def-label field) (field-def-key field)))
        (constraints (field-def-constraints field)))
    (with-output-to-string (out)
      (if (not (field-def-required field))
          ;; Optional
          (format out "~A~A: super::optional_field::OptionalField::new(~S, Default::default()),~%"
                  indent rname label)
          ;; Required — construct with default
          (etypecase ref
            (primitive-ref
             (ecase (primitive-ref-prim ref)
               (:string
                (format out "~A~A: super::primitives::TextInput::new(~S, true),~%"
                        indent rname label))
               (:int
                (format out "~A~A: {~%" indent rname)
                (format out "~A    let mut w = super::primitives::NumericInput::new(~S, true);~%" indent label)
                (emit-int-constraints out indent constraints)
                (when default-val
                  (format out "~A    w.input = tui_input::Input::new(~S.into());~%" indent
                          (format nil "~A" default-val)))
                (format out "~A    w~%" indent)
                (format out "~A},~%" indent))
               (:float
                (format out "~A~A: {~%" indent rname)
                (format out "~A    let mut w = super::primitives::FloatInput::new(~S, true);~%" indent label)
                (emit-float-constraints out indent constraints)
                (when default-val
                  (format out "~A    w.input = tui_input::Input::new(~S.into());~%" indent
                          (format nil "~A" default-val)))
                (format out "~A    w~%" indent)
                (format out "~A},~%" indent))
               (:bool
                (format out "~A~A: super::primitives::Toggle::new(~S, ~A),~%"
                        indent rname label (if default-val "true" "false")))
               (:any
                (format out "~A~A: super::primitives::JsonValueEditor::new(~S),~%"
                        indent rname label))))
            (named-ref
             (let ((td (gethash (named-ref-name ref) (schema-types schema))))
               (etypecase td
                 (enum-type
                  (format out "~A~A: super::~A::~A_select(~S),~%"
                          indent rname
                          (type-name-to-filename (named-ref-name ref))
                          (camel-to-snake (named-ref-name ref))
                          label))
                 ((or object-type union-type)
                  (format out "~A~A: Default::default(),~%" indent rname)))))
            (list-ref
             (let ((min-items (or (getf constraints :min-items) 0)))
               (format out "~A~A: super::list_editor::ListEditor::new(~S, ~D),~%"
                       indent rname label min-items)))
            (map-ref
             (format out "~A~A: super::kv_editor::KvEditor::new(~S),~%"
                     indent rname label)))))))

;;; ─── Root form ──────────────────────────────────────────────────────

(defun render-root-form-rs (schema)
  "Emit root_form.rs — the top-level application shell.
   Uses a static template with ROOT_NAME and ROOT_FILE substituted at emit time."
  (let* ((root-name (schema-root schema))
         (root-file (type-name-to-filename root-name)))
    (replace-placeholders *root-form-template*
                          `(("ROOT_NAME" . ,root-name)
                            ("ROOT_FILE" . ,root-file)))))

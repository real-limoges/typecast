(defsystem #:typecast
  :description "Schema-driven code generator with pluggable backends"
  :version "0.2.0"
  :depends-on (#:yason #:alexandria)
  :serial t
  :components
  ((:module "src/core"
    :serial t
    :components ((:file "packages")
                 (:file "util")
                 (:file "ast")
                 (:file "parse")
                 (:file "validate")))
   (:module "src/rust-ratatui"
    :serial t
    :components ((:file "packages")
                 (:file "naming")
                 (:file "render-types")
                 (:file "render-shared")
                 (:file "render-components")
                 (:file "emit")
                 (:file "main")))))

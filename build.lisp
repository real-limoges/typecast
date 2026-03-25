;;;; Build script — produces a standalone `typecast` binary.
;;;;
;;;; Usage:
;;;;   sbcl --load build.lisp
;;;;
;;;; Output:
;;;;   ./typecast  (standalone executable)

(require :asdf)

;; Load quicklisp if available (for dependencies)
(let ((ql-setup (merge-pathnames "quicklisp/setup.lisp"
                                 (user-homedir-pathname))))
  (when (probe-file ql-setup)
    (load ql-setup)))

;; Register this directory as an ASDF source
(push (uiop:getcwd) asdf:*central-registry*)

;; Load the system
(asdf:load-system :typecast)

;; Dump executable
(let ((output (merge-pathnames "typecast" (uiop:getcwd))))
  (format t "~%Building executable: ~A~%" output)
  (sb-ext:save-lisp-and-die
   output
   :toplevel #'typecast/rust-ratatui::cli-main
   :executable t
   :compression t))

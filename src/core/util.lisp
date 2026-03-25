(in-package #:typecast/core)

;;;; Generic string utilities

(defun camel-to-snake (s)
  "Convert camelCase to snake_case.  \"targetRpm\" -> \"target_rpm\"."
  (with-output-to-string (out)
    (loop for i from 0 below (length s)
          for c = (char s i)
          do (when (and (upper-case-p c) (> i 0))
               (write-char #\_ out))
             (write-char (char-downcase c) out))))

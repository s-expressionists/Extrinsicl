(in-package #:extrinsicl)

(defun install-cl (client environment)
  (define-special-operators client environment)
  (define-simple-function-aliases client environment)
  (define-simple-class-aliases client environment)
  (define-simple-constant-aliases client environment)
  (define-variables client environment)
  (define-environment-accessors client environment))

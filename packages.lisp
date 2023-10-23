(defpackage #:extrinsicl
  (:use #:cl)
  (:shadow #:get-setf-expansion #:symbol-value #:macroexpand-1 #:macroexpand)
  (:export #:install-cl)
  (:export #:get-setf-expansion #:macroexpand-1 #:macroexpand
           #:symbol-value
           #:process-type-specifier))

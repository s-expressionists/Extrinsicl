(in-package #:extrinsicl)

(defparameter *valued-variables*
  '(;; (variable initial-value)
    ;; I-D marks implementation-defined values, which I have tried to make reasonable.
    (* nil) (** nil) (*** nil) ; I-D
    (*break-on-signals* nil)
    (*compile-file-pathname* nil) (*compile-file-truename* nil)
    (*compile-print* nil) (*compile-verbose* nil) ; I-D
    (*debugger-hook* nil)
    (*features* (:ansi-cl :common-lisp)) ; I-D
    (*gensym-counter* 0) ; I-D
    (*load-pathname* nil) (*load-truename* nil) (*load-print* nil)
    (*load-verbose* nil) ; I-D
    (*macroexpand-hook* funcall) ; I-D-ish
    (*modules* nil) ; I-D
    (*package* #.(find-package "COMMON-LISP-USER"))
    (*print-array* t) ; I-D
    (*print-base* 10) (*print-case* :upcase) (*print-circle* nil)
    (*print-escape* t) (*print-gensym* t) (*print-length* nil) (*print-level* nil)
    (*print-lines* nil) (*print-radix* nil) (*print-readably* nil) (*print-right-margin* nil)
    (*print-miser-width* nil) ; I-D
    (*print-pretty* t) ; I-D
    (*read-base* 10) (*read-eval* t) (*read-suppress* nil)
    (*read-default-float-format* single-float)
    (+ nil) (++ nil) (+++ nil) (- nil) (/ nil) (// nil) (/// nil))) ; I-D

(defparameter *alias-variables*
  ;; These have implementation dependent initial values that we grab from the host.
  '(*debug-io* *default-pathname-defaults* *error-output* *print-pprint-dispatch*
    *query-io* *random-state* *readtable* *standard-input* *standard-output*
    *terminal-io* *trace-output*))

(defun define-variables (client environment)
  (loop for (name value) in *valued-variables*
        do (clostrum:make-parameter client environment name value))
  (loop for name in *alias-variables*
        do (clostrum:make-parameter client environment name (symbol-value name))))

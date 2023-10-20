(in-package #:extrinsicl)

;;;; We can't expand HANDLER-BIND or RESTART-BIND portably.
;;;; But we do import host condition functions out of pragmatism.
;;;; Depending on what you're doing with this system, you might have to override
;;;; some or all of these definitions.

(defun install-condition-system-aliases (client environment)
  (flet ((coerce-to-condition (datum arguments simple-type)
           (etypecase datum
             (condition (check-type arguments null) datum)
             ((or string function) ; format control
              (make-condition simple-type
                              :format-control datum :format-arguments arguments))
             (t (apply #'make-condition (resolve-type client environment datum)
                       arguments)))))
    (defaliases client environment
      (error (datum &rest arguments)
        (error (coerce-to-condition datum arguments 'simple-error)))
      (cerror (continue-format-control datum &rest arguments)
        (cerror continue-format-control (coerce-to-condition datum arguments 'simple-error)))
      (signal (datum &rest arguments)
        (signal (coerce-to-condition datum arguments 'simple-condition)))
      (warn (datum &rest arguments)
        (warn (coerce-to-condition datum arguments 'simple-warning)))
      (make-condition (type &rest arguments)
        (apply #'make-condition (resolve-type client environment type) arguments)))))

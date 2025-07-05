(in-package #:extrinsicl)

(defclass extrinsicl-format-client (inravina-extrinsic:extrinsic-client
                                    quaviver/schubfach:client)
  ((%client :initarg :client :reader client)
   (%environment :initarg :environment :reader environment)))

(defmethod invistra:coerce-function-designator ((client extrinsicl-format-client) object)
  (etypecase object
    (function object)
    (t (clostrum:fdefinition (client client) (environment client) object))))

(defun install-format (client environment)
  (let ((fmt-client (make-instance 'extrinsicl-format-client
                      :client client :environment environment)))
    (defaliases client environment
      (format (destination control-string &rest format-arguments)
        (let ((incless-extrinsic:*client* fmt-client)
              (destination
                (if (eql destination 't)
                    (funcall (clostrum:fdefinition client environment 'cl:symbol-value)
                             '*standard-output*)
                    destination)))
          (apply #'invistra-extrinsic:format destination control-string format-arguments))))))

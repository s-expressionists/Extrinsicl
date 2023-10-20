(in-package #:extrinsicl)

(defun install-make-load-form-saving-slots (client environment)
  (defaliases client environment
    (make-load-form-saving-slots (object &key (slot-names nil snp) environment)
      (declare (ignore environment))
      (let ((class (class-of object))
            (inits ()))
        (dolist (slotd (closer-mop:class-slots (class-of object)))
          (let ((name (closer-mop:slot-definition-name slotd)))
            (when (if snp
                      (member name slot-names)
                      ;; "If slot-names is not supplied, its value is all of the local slots."
                      (eq :instance (closer-mop:slot-definition-allocation slotd)))
              (push (if (slot-boundp object name)
                        `(setf (slot-value ,object ',name) ',(slot-value object name))
                        `(slot-makunbound ,object ',name))
                    inits))))
        (values `(allocate-instance ',class)
                (if inits `(progn ,@inits) nil))))))

(in-package #:extrinsicl)

(defgeneric process-type-specifier (client environment type-specifier)
  (:method (client type-specifier environment)
    (resolve-type client environment type-specifier))
  (:documentation "Given a type specifier and environment, return an object to store in a Trucler description.
The default method returns the type specifier after macroexpansion in the environment."))

(defun install-proclaim (client environment compilation-environment)
  (labels
      ((vardesc (name)
         (clostrum:variable-description client compilation-environment name))
       ((setf vardesc) (desc name)
         (setf (clostrum:variable-description client compilation-environment name) desc))
       (fundesc (name)
         (clostrum:function-description client compilation-environment name))
       ((setf fundesc) (desc name)
         (setf (clostrum:function-description client compilation-environment name) desc))
       (+declaration (identifier)
         (check-type identifier symbol)
         (setf (clostrum:proclamation client environment identifier) identifier))
       (set-inline (name inline)
         (let ((existing (fundesc name)))
           (setf (fundesc name)
                 (cond (existing
                        (check-type existing trucler:global-function-description)
                        (trucler:merge-inline client existing inline))
                       ;; FIXME: It is legal to proclaim macros inline or notinline
                       ;; (for the sake of compiler macros) and to do so before
                       ;; the actual macro is defined, which will cause a conflict
                       ;; if we make a global function description like this.
                       (t (make-instance 'trucler:global-function-description
                            :name name :inline inline))))))
       (+inline (name) (set-inline name 'inline))
       (+notinline (name) (set-inline name 'notinline))
       (+special (symbol)
         (clostrum:make-variable client environment symbol))
       (+optimize (optimize)
         (destructuring-bind (quality n)
             (if (symbolp optimize)
                 `(,optimize 3)
                 optimize)
           (setf (clostrum:optimize-description client compilation-environment)
                 (funcall
                  (ecase quality
                    ((compilation-speed) #'trucler:merge-compilation-speed)
                    ((debug) #'trucler:merge-debug)
                    ((safety) #'trucler:merge-safety)
                    ((space) #'trucler:merge-space)
                    ((speed) #'trucler:merge-speed))
                  client
                  (clostrum:optimize-description client compilation-environment)
                  n))))
       (typer (type-specifier)
         (let ((type (process-type-specifier client environment type-specifier)))
           (lambda (symbol)
             (let ((existing (vardesc symbol)))
               ;; FIXME: When we don't know whether this is a special or macro or what
               ;; we can't exactly stick in a description, so we ignore the proclamation.
               (when existing
                 (setf (vardesc symbol) (trucler:merge-type client existing type)))))))
       (ftyper (type-specifier)
         (let ((type (process-type-specifier client environment type-specifier)))
           (lambda (name)
             (setf (fundesc name)
                   (let ((existing (fundesc name)))
                     (if existing
                         (trucler:merge-type client existing type)
                         (make-instance 'trucler:global-function-description
                           :name name :type type)))))))
       (#1=#:proclaim (declaration-specifier)
         ;; for any nonstandard identifiers, you'll need to define your own proclaim.
         (ecase declaration-specifier
           ((declaration) (mapc #'+declaration (rest declaration-specifier)))
           ((inline) (mapc #'+inline (rest declaration-specifier)))
           ((notinline) (mapc #'+notinline (rest declaration-specifier)))
           ((special) (mapc #'+special (rest declaration-specifier)))
           ((optimize) (mapc #'+optimize (rest declaration-specifier)))
           ((type) (mapc (typer (second declaration-specifier))
                         (cddr declaration-specifier)))
           ((ftype) (mapc (ftyper (second declaration-specifier))
                          (cddr declaration-specifier))))))
    (setf (clostrum:fdefinition client environment 'proclaim) #'#1#))
  (values))

(defpackage #:extrinsicl.maclina
  (:use #:cl)
  (:export #:install-eval))

(in-package #:extrinsicl.maclina)

(defun install-eval (client environment)
  (labels
      ((fdef (name) (clostrum:fdefinition client environment name))
       ((setf fdef) (fun name)
         (setf (clostrum:fdefinition client environment name) fun))
       (%compile (definition)
         (etypecase definition
           ((cons (eql lambda))
            (maclina.compile:compile definition environment client))
           ((or compiled-function maclina.machine:bytecode-function
              maclina.machine:bytecode-closure)
            (values definition nil nil))
           (function
            (error "Don't know how to compile interpreted function ~s" definition))))
       (#1=#:compile (name &optional (definition (fdef name)))
         (if name
             (multiple-value-bind (function warningsp failurep) (%compile definition)
               (when function (setf (fdef name) function))
               (values name warningsp failurep))
             (%compile definition)))
       (#2=#:eval (form)
         (maclina.compile:eval form environment client))
       (#3=#:symbol-value (symbol)
         (maclina.machine:symbol-value client environment symbol))
       ((setf #3#) (value symbol)
         (setf (maclina.machine:symbol-value client environment symbol) value))
       (#4=#:set (symbol value)
         (setf (maclina.machine:symbol-value client environment symbol) value))
       (#5=#:makunbound (symbol)
         (maclina.machine:makunbound client environment symbol))
       (#6=#:boundp (symbol)
         (maclina.machine:boundp client environment symbol))
       (#7=#:compile-file (input-file &rest keys &key &allow-other-keys)
         ;; We default a few parameters, but let the caller override em.
         (multiple-value-call #'maclina.compile-file:compile-file
           input-file
           (values-list keys)
           :client client :environment environment
           :verbose (#3# '*compile-verbose*) :print (#3# '*compile-print*)))
       (#8=#:compile-file-pathname (input-file &rest keys &key &allow-other-keys)
         (multiple-value-call #'maclina.compile-file:compile-file-pathname
           input-file keys
           ;; unlikely this will matter, but better safe than sorry
           :client client :environment environment
           :verbose (#3# '*compile-verbose*) :print (#3# '*compile-print*)))
       (%disassemble (fn)
         (etypecase fn
           ((or maclina.machine:bytecode-function maclina.machine:bytecode-closure)
            (maclina.machine:disassemble fn))
           (function
            (format t "; Don't know how to disassemble non-bytecode function"))
           ((cons (eql lambda))
            (maclina.machine:disassemble
             (maclina.compile:compile fn environment client)))
           (t ; function name
            (%disassemble (fdef fn)))))
       (#9=#:disassemble (fn)
         (let ((*standard-output* (#3# '*standard-output*)))
           (%disassemble fn))))
    (declare (inline fdef (setf fdef)))
    ;; FIXME: LOAD is tricky since it might get a source file, and also we need to
    ;; differentiate between source files and FASLs. Arguably that should all be
    ;; in Maclina?
    (setf (fdef 'compile) #'#1# (fdef 'eval) #'#2#
          (fdef 'symbol-value) #'#3# (fdef '(setf symbol-value)) #'(setf #3#)
          (fdef 'set) #'#4# (fdef 'makunbound) #'#5# (fdef 'boundp) #'#6#
          (fdef 'compile-file) #'#7# (fdef 'compile-file-pathname) #'#8#
          (fdef 'disassemble) #'#9#))
  (flet ((defconst (name value)
           (clostrum:make-constant client environment name value)))
    (declare (inline defconst))
    (defconst 'call-arguments-limit (maclina.machine:call-arguments-limit client))
    (defconst 'lambda-list-keywords (maclina.machine:lambda-list-keywords client))
    (defconst 'lambda-parameters-limit (maclina.machine:lambda-parameters-limit client))
    (defconst 'multiple-values-limit (maclina.machine:multiple-values-limit client)))
  (values))

(defmethod extrinsicl:symbol-value ((client maclina.vm-cross:client) env symbol)
  (maclina.machine:symbol-value client env symbol))
(defmethod (setf extrinsicl:symbol-value) (new (client maclina.vm-cross:client) env symbol)
  (setf (maclina.machine:symbol-value client env symbol) new))

(defmethod common-macro-definitions:get-setf-expansion
    ((client maclina.vm-cross:client) place &optional env)
  ;; This method is only ever called by common macros on an actual environment,
  ;; which is good because that means this method doesn't need to close over
  ;; the global runtime environment for the case of env = nil.
  (assert env)
  (let* ((hook (maclina.machine:symbol-value client env '*macroexpand-hook*))
         (fhook (etypecase hook
                  (function hook)
                  (symbol
                   (clostrum:fdefinition client (trucler:global-environment client env)
                                         hook)))))
    (extrinsicl:get-setf-expansion client env fhook place)))

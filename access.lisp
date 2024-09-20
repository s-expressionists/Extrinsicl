(in-package #:extrinsicl)

(defgeneric symbol-value (client environment symbol))
(defgeneric (setf symbol-value) (value client environment symbol))

(defun fdesignator (client environment designator)
  (etypecase designator
    (function designator)
    (symbol (clostrum:fdefinition client environment designator))))

(defgeneric macroexpand-1 (client environment hook form))

(defmethod macroexpand-1 (client env (hook function) (form symbol))
  (let ((info (trucler:describe-variable client env form)))
    (if (typep info 'trucler:symbol-macro-description)
        (let ((expansion (trucler:expansion info)))
          (values
           (funcall hook (lambda (form env) (declare (ignore form env)) expansion) form env)
           t))
        (values form nil))))

(defmethod macroexpand-1 (client env (hook function) (form cons))
  (let ((head (car form)))
    (if (symbolp head)
        (let ((info (trucler:describe-function client env head)))
          (if (typep info 'trucler:macro-description)
              (values (funcall hook (trucler:expander info) form env) t)
              (values form nil)))
        (values form nil))))

(defun macroexpand (client environment hook form)
  (loop with ever-expanded-p = nil
        do (multiple-value-bind (expansion expandedp)
               (macroexpand-1 client environment hook form)
             (if expandedp
                 (setf form expansion ever-expanded-p t)
                 (return (values form ever-expanded-p))))))

(defun default-symbol-setf-expansion (symbol)
  (let ((store (gensym "STORE"))) (values () () `(,store) `(setq ,symbol ,store) symbol)))

(defun default-cons-setf-expansion (cons)
  (let* ((head (car cons)) (args (cdr cons))
         (temps (loop for arg in args
                      when (symbolp arg)
                        collect (gensym (symbol-name arg))
                      else collect (gensym "TEMP")))
         (store (gensym "STORE")))
    (values temps args (list store)
            `(funcall #'(setf ,head) ,store ,@temps) `(,head ,@temps))))

;;; Implements CL:GET-SETF-EXPANSION.
;;; This is exported so that it can be used in a method on
;;; COMMON-MACRO-DEFINITIONS:GET-SETF-EXPANSION; we can't provide that method ourselves
;;; since we don't know the client class and don't have the global environment until
;;; something provides it.
(defgeneric get-setf-expansion (client environment hook place))

(defmethod get-setf-expansion (client environment hook (place symbol))
  (multiple-value-bind (expansion expandedp)
      (macroexpand-1 client environment hook place)
    (if expandedp
        (get-setf-expansion client environment hook expansion)
        (default-symbol-setf-expansion place))))

(defun setf-expander (client environment operator)
  (if (typep (trucler:describe-function client environment operator)
             '(or trucler:local-function-description
               trucler:local-macro-description))
      nil ; shadowed
      (clostrum:setf-expander
       client (trucler:global-environment client environment) operator)))

(defmethod get-setf-expansion (client environment hook (place cons))
  (let ((head (car place)))
    (unless (symbolp head) (error "Invalid place: ~s" place)) ; FIXME better error
    (let ((expander (setf-expander client environment head)))
      (if expander
          (funcall expander place environment)
          (multiple-value-bind (expansion expandedp)
              (macroexpand-1 client environment hook place)
            (if expandedp
                (get-setf-expansion client environment hook expansion)
                (default-cons-setf-expansion place)))))))

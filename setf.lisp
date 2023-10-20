(in-package #:extrinsicl)

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
(defun get-setf-expansion (client environment place)
  (etypecase place
    (symbol (multiple-value-bind (expansion expandedp)
                (clostrum:macroexpand-1 client environment place)
              (if expandedp
                  (get-setf-expansion client environment expansion)
                  (default-symbol-setf-expansion place))))
    (cons (let ((head (car place)))
            (unless (symbolp head) (error "Invalid place: ~s" place)) ; FIXME better error
            (let ((expander (clostrum:setf-expander client environment head)))
              (if expander
                  (funcall expander place environment)
                  (multiple-value-bind (expansion expandedp)
                      (clostrum:macroexpand-1 client environment place)
                    (if expandedp
                        (get-setf-expansion client environment expansion)
                        (default-cons-setf-expansion place)))))))))

(in-package #:extrinsicl)

(defun install-compilation-cl (client compilation-environment)
  ;; CLHS 11.1.2.1.1 guarantees that CL symbols are only fbound if they
  ;; are defined to be so in this standard. So we can just copy things in.
  ;; But we don't copy in local functions because they may or may not
  ;; be globally fbound.
  ;; We also use this for variables. I don't think it's technically allowed to
  ;; locally make standard variables unbound via progv, but even if it is that's
  ;; not something I expect to be a serious issue.
  ;; TODO: Types, maybe?
  (do-external-symbols (s :cl)
    (when (and (cl:fboundp s) (not (cl:macro-function s)) (not (cl:special-operator-p s))
            (not (member s '(next-method-p call-next-method))))
      (setf (clostrum:function-description client compilation-environment s)
            (make-instance 'trucler:global-function-description :name s)))
    (when (and (cl:boundp s) (not (cl:constantp s)))
      (setf (clostrum:variable-description client compilation-environment s)
            (make-instance 'trucler:global-special-variable-description :name s))))
  ;; Special operators we list explicitly since implementations can make any macro
  ;; into a special operator.
  (dolist (op *special-operators*)
    (setf (clostrum:function-description client compilation-environment op)
          (make-instance 'trucler:special-operator-description :name op)))
  (values))

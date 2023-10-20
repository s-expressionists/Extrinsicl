(in-package #:extrinsicl)

(defparameter *special-operators*
  ;; from CLHS figure 3-2
  '(block      let*                  return-from
    catch      load-time-value       setq
    eval-when  locally               symbol-macrolet
    flet       macrolet              tagbody
    function   multiple-value-call   the
    go         multiple-value-prog1  throw
    if         progn                 unwind-protect
    labels     progv
    let        quote))

(defun install-special-operators (client environment)
  ;; from figure 3-2
  (loop for op in *special-operators*
        do (clostrum:make-special-operator client environment op t)))

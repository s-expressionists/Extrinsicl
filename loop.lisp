(in-package #:extrinsicl)

(defun define-loop (client environment)
  (loop for name in '(loop loop-finish)
        for s = (find-symbol (symbol-name name) "KHAZERN-EXTRINSIC")
        for f = (macro-function s)
        do (setf (clostrum:macro-function client environment name) f)))

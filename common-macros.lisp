(in-package #:extrinsicl)

(defparameter *common-macros*
  '(when unless and or cond case ecase ccase incf decf push pop return
    multiple-value-bind multiple-value-list multiple-value-setq nth-value
    shiftf rotatef defmethod defvar defpackage defparameter defmacro
    define-compiler-macro defun defclass define-condition defgeneric defsetf
    deftype destructuring-bind declaim lambda prog prog* prog1 prog2
    pushnew remf handler-case ignore-errors in-package check-type dolist
    dotimes do do* setf psetf psetq restart-case typecase etypecase ctypecase
    with-accessors with-input-from-string with-open-file with-open-stream
    with-output-to-string with-simple-restart with-slots
    do-symbols do-external-symbols do-all-symbols))

(defun install-common-macros (client environment)
  (loop for name in *common-macros*
        for f = (common-macro-definitions:macro-function name)
        when f
          do (setf (clostrum:macro-function client environment name)
                   (let ((f f))
                     (declare (type (function (t t)) f))
                     (lambda (form environment)
                       (let ((common-macro-definitions:*client* client))
                         (funcall f form environment)))))))

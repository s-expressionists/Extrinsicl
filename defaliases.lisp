(in-package #:extrinsicl)

;;; Helper macro for defining a bunch of alias functions.
;;; client and environment are evaluated into the client and environment.
;;; aliases are defined in an flet with proxy symbols, and then these proxy
;;; functions are loaded into the given environment.

(defmacro defaliases (client environment &body aliases)
  (let ((fnames
          (loop for (name) in aliases
                collect (etypecase name
                          (symbol (make-symbol (symbol-name name)))
                          ((cons (eql setf) (cons symbol null))
                           `(setf ,(make-symbol (symbol-name (second name))))))))
        (csym (gensym "CLIENT")) (esym (gensym "ENVIRONMENT")))
    `(let ((,csym ,client) (,esym ,environment))
       (flet
           (,@(loop for (name lambda-list . body) in aliases
                    for fname in fnames
                    collect `(,fname ,lambda-list ,@body)))
         (loop for n in '(,@(loop for (name) in aliases collect name))
               for f in (list ,@(loop for fname in fnames collect `(function ,fname)))
               do (setf (clostrum:fdefinition ,csym ,esym n) f))))))

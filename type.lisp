(in-package #:extrinsicl)

;;; A sort of macroexpand-all for type specifiers, to get anything particular to our
;;; environment out of it.
(defun resolve-type (client environment type-specifier)
  (flet ((recur (spec) (resolve-type client environment spec)))
    (let ((tspec (clostrum:type-expand client environment type-specifier)))
      (etypecase tspec
        (class tspec)
        (symbol
         (or (clostrum:find-class client environment tspec nil)
             ;; nothing much else we can do, and we shouldn't need to alias any of
             ;; the standard atomic type specifiers.
             tspec))
        (cons
         (case (car tspec)
           ((and) `(and ,@(mapcar #'recur (cdr tspec))))
           ((or) `(or ,@(mapcar #'recur (cdr tspec))))
           ((not) `(not ,(recur (second tspec))))
           ((array simple-array vector)
            (destructuring-bind (head &optional (et '*) (dims '*)) tspec
              `(,head ,(if (eq et '*) et (recur et)) ,dims)))
           ((complex)
            (destructuring-bind (&optional (et '*)) (rest tspec)
              `(complex ,(if (eq et '*) (recur et)))))
           ((cons)
            (destructuring-bind (&optional (car 't) (cdr 't)) (rest tspec)
              `(cons ,(recur car) ,(recur cdr))))
           ((function) (error "TODO: Implement complex function types"))
           ((values) (error "TODO: Implement complex values types"))
           ((satisfies) `(satisfies ,(fake-satisfies-fname (second tspec))))
           ;; These don't have sub specifiers, so just pass through
           ((base-string bit-vector double-float eql float integer
                         long-float member mod rational real short-float
                         signed-byte simple-base-string simple-bit-vector
                         simple-string simple-vector single-float string
                         unsigned-byte)
            tspec)
           ;; Unknown type.
           (otherwise tspec)))))))

(defun fake-satisfies-fname (client environment fname)
  ;; In order to make SATISFIES work, we use this horrible hack: We make a new global host
  ;; function that calls the environment's function on the object, and use that
  ;; in the type specifier.
  (let ((sym (gensym "SATISFIES-PROXY")))
    (setf (fdefinition sym)
          (lambda (object)
            (funcall (clostrum:fdefinition client environment fname) object)))
    sym))

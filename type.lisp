(in-package #:extrinsicl)

;;; A sort of macroexpand-all for type specifiers, to get anything particular to
;;; our environment out of it. We also make sure to only end up with standard
;;; specifiers, so that types from the host can't sneak in.
(defvar *atomic-type-specifiers* ; figure 4-2
  '(arithmetic-error                  function            simple-condition
    array                             generic-function    simple-error
    atom                              hash-table          simple-string
    base-char                         integer             simple-type-error
    base-string                       keyword             simple-vector
    bignum                            list                simple-warning
    bit                               logical-pathname    single-float
    bit-vector                        long-float          standard-char
    broadcast-stream                  method              standard-class
    built-in-class                    method-combination  standard-generic-function
    cell-error                        nil                 standard-method
    character                         null                standard-object
    class                             number              storage-condition
    compiled-function                 package             stream
    complex                           package-error       stream-error
    concatenated-stream               parse-error         string
    condition                         pathname            string-stream
    cons                              print-not-readable  structure-class
    control-error                     program-error       structure-object
    division-by-zero                  random-state        style-warning
    double-float                      ratio               symbol
    echo-stream                       rational            synonym-stream
    end-of-file                       reader-error        t
    error                             readtable           two-way-stream
    extended-char                     real                type-error
    file-error                        restart             unbound-slot
    file-stream                       sequence            unbound-variable
    fixnum                            serious-condition   undefined-function
    float                             short-float         unsigned-byte
    floating-point-inexact            signed-byte         vector
    floating-point-invalid-operation  simple-array        warning
    floating-point-overflow           simple-base-string
    floating-point-underflow          simple-bit-vector))

(defun resolve-type (client environment type-specifier)
  (let* ((environment (trucler:global-environment client environment))
         (tspec (clostrum:type-expand client environment type-specifier)))
    (flet ((recur (spec) (resolve-type client environment spec)))
      (etypecase tspec
        (class tspec)
        (symbol
         (if (member tspec *atomic-type-specifiers*)
             ;; nothing much else we can do, and we shouldn't need to
             ;; alias any of the standard atomic type specifiers.
             tspec
             (or (clostrum:find-class client environment tspec nil)           
               (error "Unknown type specifier: ~s" tspec))))
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
           ((satisfies)
            `(satisfies ,(fake-satisfies-fname client environment (second tspec))))
           ;; These don't have sub specifiers, so just pass through
           ((base-string bit-vector double-float eql float integer
                         long-float member mod rational real short-float
                         signed-byte simple-base-string simple-bit-vector
                         simple-string simple-vector single-float string
                         unsigned-byte)
            tspec)
           (otherwise (error "Unknown type specifier: ~s" tspec))))))))

(defun fake-satisfies-fname (client environment fname)
  ;; In order to make SATISFIES work, we use this horrible hack: We make a new global host
  ;; function that calls the environment's function on the object, and use that
  ;; in the type specifier.
  (let ((sym (gensym "SATISFIES-PROXY")))
    (setf (fdefinition sym)
          (lambda (object)
            (funcall (clostrum:fdefinition client environment fname) object)))
    sym))

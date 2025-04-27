(in-package #:extrinsicl)

;;; CLHS 5.1.1.2 defines that for a standard accessor F, it is undefined how use of it as a
;;; place is accomplished unless otherwise noted, i.e. it is undefined whether (SETF F) has
;;; to be defined. This is annoying for us because it means that we can't write out a setf
;;; expander for, say, AREF, and expect the expansion to work on every implementation
;;; without an accessory function.
;;; We define SETF functions for every accessor we can in the standard. That works within
;;; this system, but keep in mind that you're compiling code to be loaded in some other
;;; environment, you'll have to do one of the following:
;;; 1) define, in the loader environment, (setf foo) for all foo used
;;; 2) define your implementation-specific setf expanders within your compilation environment.

;;; None of this applies to the few accessors that cannot be defined as functions.
;;; For all of these, we can, happily enough, devise portable expansions.
;;; They are defined by install-setf-expanders below.

;;; Also note that we do not define (setf symbol-value) for the same reason we don't
;;; install symbol-value. But we can define a setf expander using SET.

(defun install-setf-functions (client environment)
  (defaliases client environment
    ;; Functional accessors in earlier chapters (e.g. fdefinition) are
    ;; defined in environment-access-functions.lisp.
    ;; 14 Conses
    ((setf getf) (new place indicator &optional default)
     (setf (getf place indicator default) new))
    ;; 15 Arrays
    ((setf aref) (new array &rest indices) (setf (apply #'aref array indices) new))
    ((setf fill-pointer) (new vector) (setf (fill-pointer vector) new))
    ((setf row-major-aref) (new array rmindex) (setf (row-major-aref array rmindex) new))
    ((setf svref) (new vector index) (setf (svref vector index) new))
    ((setf bit) (new bit-array &rest indices) (setf (apply #'bit bit-array indices) new))
    ((setf sbit) (new bit-array &rest indices) (setf (apply #'sbit bit-array indices) new))
    ;; 16 Strings
    ((setf char) (new string index) (setf (char string index) new))
    ((setf schar) (new string index) (setf (schar string index) new))
    ;; 17 Sequences
    ((setf elt) (new sequence index) (setf (elt sequence index) new))
    ((setf subseq) (new sequence start &optional end) (setf (subseq sequence start end) new))
    ;; 18 Hash Tables
    ((setf gethash) (new key ht &optional default) (setf (gethash key ht default) new))
    ;; 19 Filenames
    ((setf logical-pathname-translations) (new host)
     (setf (logical-pathname-translations host) new))
    ;; 23 Reader
    ((setf readtable-case) (new readtable) (setf (readtable-case readtable) new))))

(defun install-setf-expanders (client environment)
  (macrolet (;; this is a simplified define-setf-expander.
             ;; we don't bother with a proper macro function since none of these
             ;; use &whole or declarations anyway.
             (define-setf (access-fn (&rest lambda-list) eparam &body body)
               (let ((reparam (or eparam (gensym "EPARAM")))
                     (place (gensym "PLACE")))
                 `(setf (clostrum:setf-expander client environment ',access-fn)
                        (lambda (,place ,reparam)
                          (declare ,@(unless eparam `((ignore ,reparam))))
                          (let (,@(when eparam
                                    `((,eparam (or ,reparam environment)))))
                            (destructuring-bind (,@lambda-list) (rest ,place)
                              ,@body))))))
             ;; Helper for caar, etc.
             (define-cons-setf (access-fn car-cdr get)
               (let ((rplac (ecase car-cdr (car 'rplaca) (cdr 'rplacd))))
                 `(define-setf ,access-fn (cons) nil
                    (let ((c (gensym "CONS")) (store (gensym "STORE")))
                      (values (list c) (list ,get) (list store)
                              (list 'progn (list ',rplac c store) store)
                              (list ',car-cdr c)))))))
    (flet ((get-setf-expansion (place env)
             (let* ((hook (symbol-value client environment '*macroexpand-hook*))
                    (hookf (fdesignator client environment hook)))
               (get-setf-expansion client env hookf place))))
      (define-setf car (cons) nil
        (let ((c (gensym "CONS")) (store (gensym "STORE")))
          (values `(,c) (list cons) `(,store)
                  `(progn (rplaca ,c ,store) ,store) `(car ,c))))
      (define-setf cdr (cons) nil
        (let ((c (gensym "CONS")) (store (gensym "STORE")))
          (values `(,c) (list cons) `(,store)
                  `(progn (rplacd ,c ,store) ,store) `(cdr ,c))))
      (define-cons-setf caar car `(car ,cons))
      (define-cons-setf cadr car `(cdr ,cons))
      (define-cons-setf cdar cdr `(car ,cons))
      (define-cons-setf cdar cdr `(cdr ,cons))
      (define-cons-setf caaar car `(caar ,cons))
      (define-cons-setf caadr car `(cadr ,cons))
      (define-cons-setf cadar car `(cdar ,cons))
      (define-cons-setf caddr car `(cddr ,cons))
      (define-cons-setf cdaar cdr `(caar ,cons))
      (define-cons-setf cdadr cdr `(cadr ,cons))
      (define-cons-setf cddar cdr `(cdar ,cons))
      (define-cons-setf cdddr cdr `(cddr ,cons))
      (define-cons-setf cdddr cdr `(cddr ,cons))
      (define-cons-setf caaaar car `(caaar ,cons))
      (define-cons-setf caaadr car `(caadr ,cons))
      (define-cons-setf caadar car `(cadar ,cons))
      (define-cons-setf caaddr car `(caddr ,cons))
      (define-cons-setf cadaar car `(cdaar ,cons))
      (define-cons-setf cadadr car `(cdadr ,cons))
      (define-cons-setf caddar car `(cddar ,cons))
      (define-cons-setf cadddr car `(cdddr ,cons))
      (define-cons-setf cdaaar cdr `(caaar ,cons))
      (define-cons-setf cdaadr cdr `(caadr ,cons))
      (define-cons-setf cdadar cdr `(cadar ,cons))
      (define-cons-setf cdaddr cdr `(caddr ,cons))
      (define-cons-setf cddaar cdr `(cdaar ,cons))
      (define-cons-setf cddadr cdr `(cdadr ,cons))
      (define-cons-setf cdddar cdr `(cddar ,cons))
      (define-cons-setf cddddr cdr `(cdddr ,cons))
      (define-setf first (cons) env (get-setf-expansion `(car ,cons) env))
      (define-setf rest (cons) env (get-setf-expansion `(cdr ,cons) env))
      (define-cons-setf second car `(cdr ,cons))
      (define-cons-setf third car `(cddr ,cons))
      (define-cons-setf fourth car `(cdddr ,cons))
      (define-cons-setf fifth car `(cddddr ,cons))
      (define-cons-setf sixth car `(cdr (cddddr ,cons)))
      (define-cons-setf seventh car `(cddr (cddddr ,cons)))
      (define-cons-setf eighth car `(cdddr (cddddr ,cons)))
      (define-cons-setf ninth car `(cddddr (cddddr ,cons)))
      (define-cons-setf tenth car `(cdr (cddddr (cddddr ,cons))))
      (define-setf nth (n list) nil
        (let ((num (gensym "N")) (c (gensym "CONS")) (store (gensym "STORE")))
          (values `(,num ,c) (list n `(nthcdr ,num ,list)) `(,store)
                  `(progn (rplaca ,c ,store) ,store) `(car ,c))))
      (define-setf cl:symbol-value (symbol) nil
        (let ((s (gensym "SYMBOL")) (store (gensym "STORE")))
          (values `(,s) (list symbol) `(,store)
                  `(set ,s ,store)
                  `(symbol-value ,s))))
      (define-setf the (type place) env
        (multiple-value-bind (vars vals stores store-form access-form)
            (get-setf-expansion place env)
          (values vars vals stores
                  `(multiple-value-bind (,@stores) (the ,type (values ,@stores))
                     ,store-form)
                  `(the ,type ,access-form))))
      (define-setf apply (function &rest spreadable-args) nil
        (if (typep function '(cons (eql function) (cons symbol null)))
            (let ((temps (loop repeat (length spreadable-args) collect (gensym "TEMP")))
                  (store (gensym "STORE")))
              (values temps spreadable-args (list store)
                      `(apply #'(setf ,(second function)) ,store ,@temps)
                      `(apply ,function ,@temps)))
            ;; FIXME: Better error
            (error "Don't know how to expand SETF: ~s"
                   `(apply ,function ,@spreadable-args))))
      ;; The byte things could be optimized by looking for the common case of
      ;; (ldb (byte ...) ...) and so on. We don't have a portable %LDB that
      ;; deals with the size and position directly, but we could transpose the
      ;; byte form into the DPB call and hope that the implementation speeds it up.
      (define-setf ldb (bytespec int) env
        (multiple-value-bind (temps vals stores store-form access-form)
            (get-setf-expansion int env)
          (let ((store (gensym "STORE-LDB"))
                (stemp (first stores))
                (btemp (gensym "BTEMP")))
            (values `(,btemp ,@temps)
                    `(,bytespec ,@vals)
                    `(,store)
                    `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                       ,store-form ,store)
                    `(ldb ,btemp ,access-form)))))
      (define-setf mask-field (bytespec int) env
        (multiple-value-bind (temps vals stores store-form access-form)
            (get-setf-expansion int env)
          (let ((store (gensym "STORE-MASK-FIELD"))
                (stemp (first stores))
                (btemp (gensym "BTEMP-MASK-FIELD")))
            (values `(,btemp ,@temps)
                    `(,bytespec ,@vals)
                    `(,store)
                    `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
                       ,store-form ,store)
                    `(mask-field ,btemp ,access-form)))))
      (define-setf values (&rest values) env
        (let ((all-vars '())
	      (all-vals '())
	      (all-stores '())
	      (all-storing-forms '())
	      (all-get-forms '()))
          (dolist (item (reverse values))
            (multiple-value-bind (vars vals stores storing-form get-form)
	        (get-setf-expansion item env)
	      ;; If a place has more than one store variable, the other ones
	      ;; are set to nil.
	      (let ((extra (rest stores)))
	        (unless (endp extra)
	          (setq vars (append extra vars)
		        vals (append (make-list (length extra)) vals)
		        stores (list (first stores)))))
	      (setq all-vars (append vars all-vars)
	            all-vals (append vals all-vals)
	            all-stores (append stores all-stores)
	            all-storing-forms (cons storing-form all-storing-forms)
	            all-get-forms (cons get-form all-get-forms))))
          (values all-vars all-vals all-stores `(values ,@all-storing-forms)
	          `(values ,@all-get-forms))))))
  (values))

(in-package #:extrinsicl)

(defmacro defaliases (client environment &body aliases)
  (let ((asyms (loop for (name) in aliases collect (make-symbol (symbol-name name))))
        (csym (gensym "CLIENT")) (esym (gensym "ENVIRONMENT")))
    `(flet
         (,@(loop for (name lambda-list . body) in aliases
                  for asym in asyms
                  collect `(,asym ,lambda-list ,@body)))
       (loop with ,csym = ,client with ,esym = ,environment
             for n in '(,@(loop for (name) in aliases collect name))
             for f in (list ,@(loop for asym in asyms collect `(function ,asym)))
             do (setf (clostrum:fdefinition ,csym ,esym n) f)))))

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

(defun define-environment-accessors (client environment)
  (labels
      ((describe-variable (name &optional env)
         (trucler:describe-variable client (or env environment) name))
       (symbol-macro-function (name &optional env)
         (let ((info (describe-variable name env)))
           (if (typep info 'trucler:symbol-macro-description)
               (let ((expansion (trucler:expansion info)))
                 (lambda (form env) (declare (ignore form env)) expansion))
               nil)))
       (describe-function (name &optional env)
         (trucler:describe-function client (or env environment) name))
       (^fdefinition (name) (clostrum:fdefinition client environment name))
       (fdesignator (desig)
         (etypecase desig
           (function desig)
           (symbol (^fdefinition desig))))
       (^macro-function (name &optional env)
         (let ((info (describe-function name env)))
           (if (typep info 'trucler:macro-description)
               (trucler:expander info)
               nil)))
       (macroexpand-hook () (fdesignator (^symbol-value '*macroexpand-hook*)))
       (^macroexpand-1 (form &optional env)
         (let ((expander
                 (typecase form
                   (symbol (symbol-macro-function form env))
                   (cons (and (symbolp (car form)) (^macro-function (car form) env)))
                   (t nil))))
           (if expander
               (values (funcall (macroexpand-hook) expander form env) t)
               (values form nil))))
       (setf-expander (name &optional env)
         (let ((info (describe-function name env)))
           (if (typep info '(or trucler:global-function-description
                             trucler:global-macro-description))
               (clostrum:setf-expander client environment name)
               nil)))
       (^get-setf-expansion (place &optional env)
         (etypecase place
           (symbol (multiple-value-bind (expansion expandedp)
                       (^macroexpand-1 place env)
                     (if expandedp
                         (^get-setf-expansion expansion env)
                         (default-symbol-setf-expansion place))))
           (cons (let ((head (car place)))
                   (unless (symbolp head) (error "Invalid place: ~s" place)) ; FIXME
                   (let ((expander (setf-expander head env)))
                     (if expander
                         (funcall expander place env)
                         (multiple-value-bind (expansion expandedp)
                             (^macroexpand-1 place env)
                           (if expandedp
                               (^get-setf-expansion expansion env)
                               (default-cons-setf-expansion place)))))))))
       (^find-class (name &optional (errorp t) env)
         (clostrum:find-class client (or env environment) name errorp))
       (class-designator (desig)
         (etypecase desig
           (class desig)
           (symbol (^find-class desig))))
       (^resolve-type (type-specifier)
         (resolve-type client environment type-specifier))
       (retest1 (function key more-keys &rest fixed)
         (declare (dynamic-extent fixed))
         (multiple-value-call function
           (values-list fixed) :key (fdesignator key) more-keys))
       (retest2 (function key test testp test-not test-not-p more-keys &rest fixed)
         (declare (dynamic-extent fixed))
         (cond ((and testp test-not-p) (error "~s and ~s both supplied" :test :test-not))
               (test-not-p
                (multiple-value-call function (values-list fixed)
                  :key (fdesignator key) :test-not (fdesignator test-not) more-keys))
               (t
                (multiple-value-call function (values-list fixed)
                  :key (fdesignator key) :test (fdesignator test) more-keys))))
       ;; since symbol-value's implementation is part of the evaluator runtime,
       ;; we can't define it in this system. But we do sometimes need to grab symbol values,
       ;; for other functions, so we assume that SYMBOL-VALUE will be defined later.
       ;; This is NOT the function symbol-value in the environment, since that would be
       ;; recursive.
       (^symbol-value (symbol)
         (funcall (clostrum:fdefinition client environment 'symbol-value) symbol))
       (default-pathname-defaults () (^symbol-value '*default-pathname-defaults*))
       (pprint-table () (^symbol-value '*print-pprint-dispatch*))
       (input-stream-designator (desig)
         (etypecase desig
           (stream desig)
           ((eql t) (^symbol-value '*terminal-io*))
           ((eql nil) (^symbol-value '*standard-input*))))
       (output-stream-designator (desig)
         (etypecase desig
           (stream desig)
           ((eql t) (^symbol-value '*terminal-io*))
           ((eql nil) (^symbol-value '*standard-output*)))))
    ;; SBCL whines about &optional &key
    #+sbcl (declare (sb-ext:muffle-conditions style-warning))
    (defaliases client environment
      ;; evaluation and compilation
      (compiler-macro-function (name &optional env)
        (let ((info (describe-function name env)))
          (if (typep info '(or trucler:global-function-description
                            trucler:global-macro-description))
              (trucler:compiler-macro info)
              nil)))
      (macro-function (name &optional env) (^macro-function name env))
      (macroexpand-1 (form &optional env) (^macroexpand-1 form env))
      (macroexpand (form &optional env)
        (loop with ever-expanded-p = nil
              do (multiple-value-bind (expansion expandedp) (^macroexpand-1 form env)
                   (if expandedp
                       (setf form expansion ever-expanded-p t)
                       (return (values form ever-expanded-p))))))
      ;; proclaim?
      ;; data and control flow
      (apply (function &rest spreadable-arguments)
        (apply #'apply (fdesignator function) spreadable-arguments))
      (fdefinition (name) (^fdefinition name))
      (fboundp (name) (clostrum:fboundp client environment name))
      (fmakunbound (name) (clostrum:fmakunbound client environment name))
      (funcall (function &rest arguments)
        (apply (fdesignator function) arguments))
      (every (predicate &rest sequences)
        (apply #'every (fdesignator predicate) sequences))
      (some (predicate &rest sequences)
        (apply #'some (fdesignator predicate) sequences))
      (notevery (predicate &rest sequences)
        (apply #'notevery (fdesignator predicate) sequences))
      (notany (predicate &rest sequences)
        (apply #'notany (fdesignator predicate) sequences))
      (get-setf-expansion (place &optional env) (^get-setf-expansion place env))
      ;; Objects
      ;; FIXME: Method combination?
      (ensure-generic-function (function-name
                                 &rest keys
                                 &key (generic-function-class 'standard-generic-function)
                                 (method-class 'standard-method)
                                 &allow-other-keys)
        (apply #'ensure-generic-function function-name
               :generic-function-class (class-designator generic-function-class)
               :method-class (class-designator method-class)
               keys))
      ;; change-class, make-instance are generic (TODO: define anyway?)
      (find-class (name &optional (errorp t) env) (^find-class name errorp env))
      ;; symbols
      ;; TODO: symbol-value etc are part of the runtime so we can't define them here,
      ;; but maybe we could do symbol-plist?
      (symbol-function (symbol)
        (check-type symbol symbol)
        (^fdefinition symbol))
      ;; Conses
      (sublis (alist tree
                     &rest keys
                     &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                     &allow-other-keys)
        (retest2 #'sublis key test testp test-not test-not-p keys alist tree))
      (nsublis (alist tree
                      &rest keys
                      &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                      &allow-other-keys)
        (retest2 #'nsublis key test testp test-not test-not-p keys alist tree))
      (subst (new old tree
                  &rest keys
                  &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                  &allow-other-keys)
        (retest2 #'subst key test testp test-not test-not-p keys new old tree))
      (subst-if (new predicate tree &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'subst-if key keys new (fdesignator predicate) tree))
      (subst-if-not (new predicate tree &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'subst-if-not key keys new (fdesignator predicate) tree))
      (nsubst (new old tree
                    &rest keys
                    &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                    &allow-other-keys)
        (retest2 #'nsubst key test testp test-not test-not-p keys new old tree))
      (nsubst-if (new predicate tree &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'nsubst-if key keys new (fdesignator predicate) tree))
      (nsubst-if-not (new predicate tree &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'nsubst-if-not key keys new (fdesignator predicate) tree))
      (tree-equal (tree1 tree2 &rest keys
                         &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                         &allow-other-keys)
        (retest2 #'tree-equal key test testp test-not test-not-p keys tree1 tree2))
      (member (item list &rest keys
                    &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                    &allow-other-keys)
        (retest2 #'member key test testp test-not test-not-p keys item list))
      (member-if (predicate list &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'member-if key keys (fdesignator predicate) list))
      (member-if-not (predicate list &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'member-if-not key keys (fdesignator predicate) list))
      (mapc (function &rest lists) (apply #'mapc (fdesignator function) lists))
      (mapcar (function &rest lists) (apply #'mapcar (fdesignator function) lists))
      (mapcan (function &rest lists) (apply #'mapcan (fdesignator function) lists))
      (mapl (function &rest lists) (apply #'mapl (fdesignator function) lists))
      (maplist (function &rest lists) (apply #'maplist (fdesignator function) lists))
      (mapcon (function &rest lists) (apply #'mapcon (fdesignator function) lists))
      (assoc (item list &rest keys
                   &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                   &allow-other-keys)
        (retest2 #'assoc key test testp test-not test-not-p keys item list))
      (assoc-if (predicate list &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'assoc-if key keys (fdesignator predicate) list))
      (assoc-if-not (predicate list &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'assoc-if-not key keys (fdesignator predicate) list))
      (rassoc (item list &rest keys
                    &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                    &allow-other-keys)
        (retest2 #'rassoc key test testp test-not test-not-p keys item list))
      (rassoc-if (predicate list &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'rassoc-if key keys (fdesignator predicate) list))
      (rassoc-if-not (predicate list &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'rassoc-if-not key keys (fdesignator predicate) list))
      (intersection (list1 list2 &rest keys
                           &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                           &allow-other-keys)
        (retest2 #'intersection key test testp test-not test-not-p keys list1 list2))
      (nintersection (list1 list2 &rest keys
                            &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                            &allow-other-keys)
        (retest2 #'nintersection key test testp test-not test-not-p keys list1 list2))
      (adjoin (item list &rest keys
                    &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                    &allow-other-keys)
        (retest2 #'adjoin key test testp test-not test-not-p keys item list))
      (set-difference (list1 list2 &rest keys
                             &key (key 'identity) (test 'eql testp)
                             (test-not nil test-not-p)
                             &allow-other-keys)
        (retest2 #'set-difference key test testp test-not test-not-p keys list1 list2))
      (nset-difference (list1 list2 &rest keys
                              &key (key 'identity) (test 'eql testp)
                              (test-not nil test-not-p)
                              &allow-other-keys)
        (retest2 #'nset-difference key test testp test-not test-not-p keys list1 list2))
      (set-exclusive-or (list1 list2 &rest keys
                               &key (key 'identity) (test 'eql testp)
                               (test-not nil test-not-p)
                               &allow-other-keys)
        (retest2 #'set-exclusive-or key test testp test-not test-not-p keys list1 list2))
      (nset-exclusive-or (list1 list2 &rest keys
                                &key (key 'identity) (test 'eql testp)
                                (test-not nil test-not-p)
                                &allow-other-keys)
        (retest2 #'nset-exclusive-or key test testp test-not test-not-p keys list1 list2))
      (subsetp (list1 list2 &rest keys
                      &key (key 'identity) (test 'eql testp)
                      (test-not nil test-not-p)
                      &allow-other-keys)
        (retest2 #'subsetp key test testp test-not test-not-p keys list1 list2))
      (union (list1 list2 &rest keys
                    &key (key 'identity) (test 'eql testp)
                    (test-not nil test-not-p)
                    &allow-other-keys)
        (retest2 #'union key test testp test-not test-not-p keys list1 list2))
      (nunion (list1 list2 &rest keys
                     &key (key 'identity) (test 'eql testp)
                     (test-not nil test-not-p)
                     &allow-other-keys)
        (retest2 #'nunion key test testp test-not test-not-p keys list1 list2))
      ;; 15 Arrays
      (make-array (dimensions &rest keys &key (element-type t) &allow-other-keys)
        (apply #'make-array dimensions :element-type (^resolve-type element-type) keys))
      (adjust-array (array dimensions &rest keys &key (element-type t) &allow-other-keys)
        (apply #'adjust-array array dimensions :element-type (^resolve-type element-type) keys))
      (upgraded-array-element-type (typespec &optional env)
        (declare (ignore env)) ; FIXME? Not sure about this
        (upgraded-array-element-type (^resolve-type typespec)))
      ;; 16 Strings
      ;; 17 Sequences
      (make-sequence (result-type size &key initial-element)
        (make-sequence (^resolve-type result-type) size :initial-element initial-element))
      (map (result-type function &rest sequences)
        (apply #'map (^resolve-type result-type) (fdesignator function) sequences))
      (map-into (result function &rest sequences)
        (apply #'map-into result (fdesignator function) sequences))
      (reduce (function sequence &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'reduce key keys function sequence))
      (count (item sequence &rest keys
                   &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                   &allow-other-keys)
        (retest2 #'count key test testp test-not test-not-p keys item sequence))
      (count-if (predicate sequence &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'count-if key keys (fdesignator predicate) sequence))
      (count-if-not (predicate sequence &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'count-if-not key keys (fdesignator predicate) sequence))
      (sort (sequence predicate &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'sort key keys sequence (fdesignator predicate)))
      (stable-sort (sequence predicate &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'stable-sort key keys sequence (fdesignator predicate)))
      (find (item sequence &rest keys
                  &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                  &allow-other-keys)
        (retest2 #'find key test testp test-not test-not-p keys item sequence))
      (find-if (predicate sequence &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'find-if key keys (fdesignator predicate) sequence))
      (find-if-not (predicate sequence &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'find-if-not key keys (fdesignator predicate) sequence))
      (position (item sequence &rest keys
                      &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                      &allow-other-keys)
        (retest2 #'position key test testp test-not test-not-p keys item sequence))
      (position-if (predicate sequence &rest keys &key (key 'identity) &allow-other-keys)
        (retest1 #'position-if key keys (fdesignator predicate) sequence))
      (position-if-not (predicate sequence &rest keys
                                  &key (key 'identity) &allow-other-keys)
        (retest1 #'position-if-not key keys (fdesignator predicate) sequence))
      (search (seq1 seq2 &rest keys
                    &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                    &allow-other-keys)
        (retest2 #'search key test testp test-not test-not-p keys seq1 seq2))
      (mismatch (seq1 seq2 &rest keys
                      &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                      &allow-other-keys)
        (retest2 #'mismatch key test testp test-not test-not-p keys seq1 seq2))
      (substitute (new old seq &rest keys
                       &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                       &allow-other-keys)
        (retest2 #'substitute key test testp test-not test-not-p keys new old seq))
      (substitute-if (new predicate sequence &rest keys
                          &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                          &allow-other-keys)
        (retest2 #'substitute-if key test testp test-not test-not-p keys
                 new (fdesignator predicate) sequence))
      (substitute-if-not (new predicate sequence &rest keys
                              &key (key 'identity)
                              (test 'eql testp) (test-not nil test-not-p)
                              &allow-other-keys)
        (retest2 #'substitute-if-not key test testp test-not test-not-p keys
                 new (fdesignator predicate) sequence))
      (nsubstitute (new old seq &rest keys
                        &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                        &allow-other-keys)
        (retest2 #'nsubstitute key test testp test-not test-not-p keys new old seq))
      (nsubstitute-if (new predicate sequence &rest keys
                           &key (key 'identity) (test 'eql testp) (test-not nil test-not-p)
                           &allow-other-keys)
        (retest2 #'nsubstitute-if key test testp test-not test-not-p keys
                 new (fdesignator predicate) sequence))
      (nsubstitute-if-not (new predicate sequence &rest keys
                               &key (key 'identity)
                               (test 'eql testp) (test-not nil test-not-p)
                               &allow-other-keys)
        (retest2 #'nsubstitute-if-not key test testp test-not test-not-p keys
                 new (fdesignator predicate) sequence))
      (concatenate (result-type &rest sequences)
        (apply #'concatenate (^resolve-type result-type) sequences))
      (merge (result-type seq1 seq2 predicate
                          &rest keys &key (key #'identity) &allow-other-keys)
        (retest1 #'merge key keys
                 (^resolve-type result-type) seq1 seq2 (fdesignator predicate)))
      (remove (item sequence &rest keys
                    &key (key #'identity) (test #'eql testp) (test-not nil test-not-p)
                    &allow-other-keys)
        (retest2 #'remove key test testp test-not test-not-p keys item sequence))
      (remove-if (test sequence &rest keys &key (key #'identity) &allow-other-keys)
        (retest1 #'remove-if key keys (fdesignator test) sequence))
      (remove-if-not (test sequence &rest keys &key (key #'identity) &allow-other-keys)
        (retest1 #'remove-if-not key keys (fdesignator test) sequence))
      (delete (item sequence &rest keys
                    &key (key #'identity) (test #'eql testp) (test-not nil test-not-p)
                    &allow-other-keys)
        (retest2 #'delete key test testp test-not test-not-p keys item sequence))
      (delete-if (test sequence &rest keys &key (key #'identity) &allow-other-keys)
        (retest1 #'delete-if key keys (fdesignator test) sequence))
      (delete-if-not (test sequence &rest keys &key (key #'identity) &allow-other-keys)
        (retest1 #'delete-if-not key keys (fdesignator test) sequence))
      (remove-duplicates (sequence &rest keys
                                   &key (key #'identity)
                                   (test #'eql testp) (test-not nil test-not-p)
                                   &allow-other-keys)
        (retest2 #'remove-duplicates key test testp test-not test-not-p keys sequence))
      (delete-duplicates (sequence &rest keys
                                   &key (key #'identity)
                                   (test #'eql testp) (test-not nil test-not-p)
                                   &allow-other-keys)
        (retest2 #'delete-duplicates key test testp test-not test-not-p keys sequence))
      ;; 18 Hash Tables
      ;; 19 Filenames
      (make-pathname (&rest keys &key &allow-other-keys)
        (let ((*default-pathname-defaults* (default-pathname-defaults)))
          (apply #'make-pathname keys)))
      (enough-namestring (pathname
                          &optional (defaults (default-pathname-defaults)))
        (enough-namestring pathname defaults))
      (parse-namestring (thing &optional host
                               (default-pathname (default-pathname-defaults))
                               &rest keys &key &allow-other-keys)
        (apply #'parse-namestring thing host default-pathname keys))
      (merge-pathnames (pathname &optional (default-pathname (default-pathname-defaults))
                                 (default-version :newest))
        (merge-pathnames pathname default-pathname default-version))
      ;; 20 Files
      ;; 21 Streams
      (peek-char (&optional peek-type input-stream (eof-error-p t) eof-value recursivep)
        (peek-char peek-type (input-stream-designator input-stream)
                   eof-error-p eof-value recursivep))
      (read-char (&optional input-stream (eof-error-p t) eof-value recursivep)
        (read-char (input-stream-designator input-stream) eof-error-p eof-value recursivep))
      (read-char-no-hang (&optional input-stream (eof-error-p t) eof-value recursivep)
        (read-char-no-hang (input-stream-designator input-stream)
                           eof-error-p eof-value recursivep))
      (terpri (&optional output-stream)
        (terpri (output-stream-designator output-stream)))
      (fresh-line (&optional output-stream)
        (fresh-line (output-stream-designator output-stream)))
      (unread-char (character &optional output-stream)
        (unread-char character (output-stream-designator output-stream)))
      (write-char (character &optional output-stream)
        (write-char character (output-stream-designator output-stream)))
      (read-line (&optional input-stream (eof-error-p t) eof-value recursivep)
        (read-line (input-stream-designator input-stream) eof-error-p eof-value recursivep))
      (write-string (string &optional output-stream &key (start 0) end)
        (write-string string (output-stream-designator output-stream) :start start :end end))
      (write-line (string &optional output-stream &key (start 0) end)
        (write-line string (output-stream-designator output-stream) :start start :end end))
      (open (filespec &rest keys &key (element-type 'character) &allow-other-keys)
        (apply #'open filespec :element-type (^resolve-type element-type) keys))
      (listen (&optional stream) (listen (input-stream-designator stream)))
      (clear-input (&optional stream) (clear-input (input-stream-designator stream)))
      (finish-output (&optional stream) (finish-output (output-stream-designator stream)))
      (force-output (&optional stream) (force-output (output-stream-designator stream)))
      (clear-output (&optional stream) (clear-output (output-stream-designator stream)))
      (y-or-n-p (&optional control &rest arguments)
        (let ((*query-io* (^symbol-value '*query-io*))) (apply #'y-or-n-p control arguments)))
      (yes-or-no-p (&optional control &rest arguments)
        (let ((*query-io* (^symbol-value '*query-io*)))
          (apply #'yes-or-no-p control arguments)))
      ;; 22 Printer
      (copy-pprint-dispatch (&optional (table (pprint-table))) (copy-pprint-dispatch table))
      (pprint-dispatch (object &optional (table (pprint-table)))
        (pprint-dispatch object table))
      ;; losing steam here
      ;; 25 Environment
      (describe (object &optional stream)
        (describe object (output-stream-designator stream))))))

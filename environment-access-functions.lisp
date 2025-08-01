(in-package #:extrinsicl)

(defun ^constantp (client environment form)
  (typecase form
    (symbol (typep (trucler:describe-variable client environment form)
                   'trucler:constant-variable-description))
    ((cons (eql quote) (cons t null)) t) ; note: assumes QUOTE has normal meaning
    (cons nil)
    (t t)))

(defun install-environment-accessors (client environment)
  (labels
      ((^symbol-value (symbol) (symbol-value client environment symbol))
       ((setf ^symbol-value) (value symbol)
         (setf (symbol-value client environment symbol) value))
       (^symbol-plist (symbol)
         (clostrum:symbol-plist client environment symbol))
       ((setf ^symbol-plist) (new symbol)
         (setf (clostrum:symbol-plist client environment symbol) new))
       (describe-function (name &optional env)
         (trucler:describe-function client (or env environment) name))
       (^fdefinition (name) (clostrum:fdefinition client environment name))
       (fdesignator (desig)
         ;; FIXME: duplicate of top level fdesignator. dumb!
         (etypecase desig
           (function desig)
           (symbol (^fdefinition desig))))
       (^macro-function (name &optional env)
         (let ((info (describe-function name env)))
           (if (typep info 'trucler:macro-description)
               (trucler:expander info)
               nil)))
       (macroexpand-hook () (fdesignator (^symbol-value '*macroexpand-hook*)))
       (^find-class (name &optional (errorp t) env)
         (clostrum:find-class
          client (trucler:global-environment client (or env environment))
          name errorp))
       (^resolve-type (type-specifier &optional env)
         (resolve-type client (or env environment) type-specifier))
       (retest1 (function key more-keys &rest fixed)
         (declare (dynamic-extent fixed))
         (multiple-value-call function
           (values-list fixed) :key (fdesignator key) (values-list more-keys)))
       (retest2 (function key test testp test-not test-not-p more-keys &rest fixed)
         (declare (dynamic-extent fixed))
         (cond ((and testp test-not-p) (error "~s and ~s both supplied" :test :test-not))
               (test-not-p
                (multiple-value-call function (values-list fixed)
                  :key (fdesignator key) :test-not (fdesignator test-not)
                  (values-list more-keys)))
               (t
                (multiple-value-call function (values-list fixed)
                  :key (fdesignator key) :test (fdesignator test)
                  (values-list more-keys)))))
       (rebind-read (thunk)
         "Call THUNK with the host reader variables rebound to the environment's."
         (let ((*read-base* (^symbol-value '*read-base*))
               (*read-default-float-format* (^symbol-value '*read-default-float-format*))
               (*read-eval* (^symbol-value '*read-eval*))
               (*read-suppress* (^symbol-value '*read-suppress*))
               (*readtable* (creadtable)))
           (funcall thunk)))
       (rebind-write (thunk)
         "Call THUNK with the host reader variables rebound to the environment's."
         (macrolet ((with-vars ((&rest vars) &body body)
                      `(let* (,@(loop for var in vars
                                      collect `(,var (^symbol-value ',var))))
                         ,@body)))
           (with-vars (*print-array* *print-base* *print-radix* *print-case*
                                     *print-circle* *print-escape* *print-gensym*
                                     *print-level* *print-length* *print-lines*
                                     *print-miser-width* *print-pprint-dispatch*
                                     *print-pretty* *print-readably* *print-right-margin*)
             (funcall thunk))))
       (current-package () (^symbol-value '*package*))
       (creadtable () (^symbol-value '*readtable*))
       (default-pathname-defaults () (^symbol-value '*default-pathname-defaults*))
       (pprint-table () (^symbol-value '*print-pprint-dispatch*))
       (current-random-state () (^symbol-value '*random-state*))
       (input-stream-designator (desig)
         (etypecase desig
           (stream desig)
           ((eql t) (^symbol-value '*terminal-io*))
           ((eql nil) (^symbol-value '*standard-input*))))
       (output-stream-designator (desig)
         (etypecase desig
           (stream desig)
           ((eql t) (^symbol-value '*terminal-io*))
           ((eql nil) (^symbol-value '*standard-output*))))
       (package-designator (desig)
         (etypecase desig
           (package desig)
           ((or string character symbol)
            (clostrum:find-package client environment (string desig))))))
    ;; SBCL whines about &optional &key
    #+sbcl (declare (sb-ext:muffle-conditions style-warning))
    (defaliases client environment
      ;; 3 Evaluation and Compilation
      (compiler-macro-function (name &optional env)
        (let ((info (describe-function name env)))
          (if (typep info '(or trucler:global-function-description
                            trucler:global-macro-description))
              (trucler:compiler-macro info)
              nil)))
      ((setf compiler-macro-function) (function name &optional env)
        (check-type env null)
        (setf (clostrum:compiler-macro-function client environment name) function))
      (macro-function (name &optional env) (^macro-function name env))
      ((setf macro-function) (function name &optional env)
        (check-type env null)
        (setf (clostrum:macro-function client environment name) function))
      (cl:macroexpand-1 (form &optional env)
        (macroexpand-1 client (or env environment) (macroexpand-hook) form))
      (cl:macroexpand (form &optional env)
        (macroexpand client (or env environment) (macroexpand-hook) form))
      (special-operator-p (name)
        (check-type name symbol)
        (typep (describe-function name) 'trucler:special-operator-description))
      (constantp (form &optional env) (^constantp client (or env environment) form))
      ;; proclaim?
      ;; 4 Types and Classes
      (coerce (object result-type)
        (let ((type (^resolve-type result-type)))
          (if (subtypep type 'function)
              (if (typep object '(cons (eql lambda)))
                  (funcall (^fdefinition 'eval) object)
                  ;; assume function name
                  ;; FIXME: This is slightly wrong - it should signal an error
                  ;; if the name is a macro function or special operator.
                  (^fdefinition object))
              (coerce object type))))
      (subtypep (ts1 ts2 &optional env)
        (subtypep (^resolve-type ts1 env) (^resolve-type ts2 env)))
      (typep (object tspec &optional env)
        (typep object (^resolve-type tspec env)))
      ;; 5 Data and Control Flow
      (apply (function &rest spreadable-arguments)
        (apply #'apply (fdesignator function) spreadable-arguments))
      (fdefinition (name) (^fdefinition name))
      ((setf fdefinition) (function name)
        (setf (clostrum:fdefinition client environment name) function))
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
      (cl:get-setf-expansion (place &optional env)
        (get-setf-expansion client (or env environment) (macroexpand-hook) place))
      ;; 7 Objects
      (find-class (name &optional (errorp t) env) (^find-class name errorp env))
      ((setf find-class) (class name &optional errorp env)
        (declare (ignore errorp env)) ; FIXME: Not sure about ignoring env.
       (setf (clostrum:find-class client environment name) class))
      ;; 9 Conditions
      (invoke-debugger (condition)
        (let ((*debugger-hook* (^symbol-value '*debugger-hook*)))
          (invoke-debugger condition)))
      ;; 10 Symbols
      (copy-symbol (symbol &optional copy-props)
        (let ((new (make-symbol (symbol-name symbol))))
          (when copy-props
            (when (funcall (clostrum:fdefinition client environment 'boundp) symbol)
              (setf (^symbol-value new) (^symbol-value symbol)))
            (when (clostrum:fboundp client environment symbol)
              (setf (clostrum:fdefinition client environment new)
                    (clostrum:fdefinition client environment symbol)))
            (setf (clostrum:symbol-plist client environment new)
                  (clostrum:symbol-plist client environment symbol)))
          new))
      (gensym (&optional (x "G"))
        (make-symbol
         (etypecase x
           (string
            (prog1
                (concatenate 'string
                             x (write-to-string (^symbol-value '*gensym-counter*) :base 10))
              (incf (^symbol-value '*gensym-counter*))))
           ((integer 0)
            (concatenate 'string "G" (write-to-string x :base 10))))))
      (gentemp (&optional prefix (package (current-package)))
        (gentemp prefix (package-designator package)))
      (symbol-function (symbol)
        (check-type symbol symbol)
        (^fdefinition symbol))
      ((setf symbol-function) (function symbol)
        (check-type symbol symbol)
        (setf (clostrum:fdefinition client environment symbol) function))
      (symbol-plist (symbol) (^symbol-plist symbol))
      ((setf symbol-plist) (plist symbol) (setf (^symbol-plist symbol) plist))
      (cl:symbol-value (symbol) (^symbol-value symbol))
      ((setf cl:symbol-value) (value symbol) (setf (^symbol-value symbol) value))
      (get (symbol indicator &optional default)
        (getf (^symbol-plist symbol) indicator default))
      ((setf get) (new symbol indicator &optional default)
        (declare (ignore default))
        (setf (getf (^symbol-plist symbol) indicator) new))
      (remprop (symbol indicator) (remf (^symbol-plist symbol) indicator))
      ;; 11 Packages
      (export (symbols &optional (package (current-package)))
        (export symbols (package-designator package)))
      (find-symbol (string &optional (package (current-package)))
        (find-symbol string (package-designator package)))
      (find-package (name) (package-designator name))
      (find-all-symbols (string)
        (let ((result nil) (name (string string)))
          (clostrum:map-all-packages client environment
                                     (lambda (p)
                                       (let ((s (find-symbol name p)))
                                         (when s (pushnew s result)))))
          result))
      (import (symbols &optional (package (current-package)))
        (import symbols (package-designator package)))
      (list-all-packages ()
        (let ((result nil))
          (clostrum:map-all-packages client environment
                                     (lambda (p) (push p result)))
          result))
      (rename-package (package new-name &optional new-nicknames)
        (let ((new-name (string new-name)))
          (setf (clostrum:package-name client environment package) new-name)
          ;; Remove any old names.
          (loop for name in (clostrum:package-names client environment package)
                do (setf (clostrum:find-package client environment name) nil))
          ;; Install new names.
          (setf (clostrum:find-package client environment new-name) package)
          (loop for nick in new-nicknames
                for snick = (string nick)
                do (setf (clostrum:find-package client environment snick) package)))
        package)
      (shadow (names &optional (package (current-package)))
        (shadow names (package-designator package)))
      (shadowing-import (symbols &optional (package (current-package)))
        (shadowing-import symbols (package-designator package)))
      (delete-package (package)
        (let ((package (package-designator package)))
          (loop for name in (clostrum:package-names client environment package)
                do (setf (clostrum:find-package client environment name) nil))
          (setf (clostrum:package-name client environment package) nil)
          (delete-package package)))
      (unexport (symbols &optional (package (current-package)))
        (unexport symbols (package-designator package)))
      (unintern (symbols &optional (package (current-package)))
        (unintern symbols package))
      (unuse-package (packages-to-unuse &optional (package (current-package)))
        (let ((to-unuse (if (listp packages-to-unuse)
                            (mapcar #'package-designator packages-to-unuse)
                            (package-designator packages-to-unuse))))
          (unuse-package to-unuse (package-designator package))))
      (use-package (packages-to-use &optional (package (current-package)))
        (let ((to-use (if (listp packages-to-use)
                          (mapcar #'package-designator packages-to-use)
                          (package-designator packages-to-use))))
          (use-package to-use (package-designator package))))
      (intern (string &optional (package (current-package)))
        (intern string (package-designator package)))
      (package-name (package)
        (clostrum:package-name client environment (package-designator package)))
      (package-nicknames (package)
        (delete (clostrum:package-name client environment package)
                (clostrum:package-names client environment package)))
      (package-shadowing-symbols (package)
        (package-shadowing-symbols (package-designator package)))
      (package-use-list (package) (package-use-list (package-designator package)))
      (package-used-by-list (package) (package-used-by-list (package-designator package)))
      ;; 12 Numbers
      (make-random-state (&optional state)
        (make-random-state (if (null state) (current-random-state) state)))
      (random (limit &optional (state (current-random-state))) (random limit state))
      (upgraded-complex-part-type (typespec &optional env)
        (upgraded-complex-part-type (^resolve-type typespec env)))
      ;; 14 Conses
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
      (adjust-array (array dimensions &rest keys &key (element-type t etp) &allow-other-keys)
        (if etp
            (apply #'adjust-array array dimensions :element-type (^resolve-type element-type) keys)
            (apply #'adjust-array array dimensions keys)))
      (upgraded-array-element-type (typespec &optional env)
        (upgraded-array-element-type (^resolve-type typespec env)))
      ;; 16 Strings
      (make-string (count &rest keys &key (element-type 'character) &allow-other-keys)
        (apply #'make-string count :element-type (^resolve-type element-type) keys))
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
      (maphash (function hash-table) (maphash (fdesignator function) hash-table))
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
      ;; These are defined to accept but ignore atsignp.
      (pprint-fill (stream object &optional (colonp t) atsignp)
        (declare (ignore atsignp))
        (rebind-write (lambda () (pprint-fill stream object colonp))))
      (pprint-linear (stream object &optional (colonp t) atsignp)
        (declare (ignore atsignp))
        (rebind-write (lambda () (pprint-linear stream object colonp))))
      (pprint-tabular (stream object &optional (colonp t) atsignp)
        (declare (ignore atsignp))
        (rebind-write (lambda () (pprint-tabular stream object colonp))))
      (pprint-indent (relative-to n &optional stream)
        (rebind-write (lambda () (pprint-indent relative-to n
                                                (output-stream-designator stream)))))
      (pprint-newline (kind &optional stream)
        (rebind-write (lambda () (pprint-newline kind (output-stream-designator stream)))))
      (pprint-tab (kind colnum colinc &optional stream)
        (rebind-write (lambda () (pprint-tab kind colnum colinc
                                             (output-stream-designator stream)))))
      ;; The standard isn't explicit about whether a function name, if used, can be
      ;; resolved immediately. Out of an abundance of caution we do delay.
      (set-pprint-dispatch (type-specifier function &optional priority (table (pprint-table)))
        (set-pprint-dispatch (^resolve-type type-specifier)
                             (typecase function
                               ((or null function) function)
                               (t ; assume a function name.
                                (lambda (stream object)
                                  (funcall (^fdefinition function) stream object))))
                             priority table))
      (write (object &rest keys &key stream &allow-other-keys)
        (rebind-write (lambda ()
                        (apply #'write :stream (output-stream-designator stream) keys))))
      (prin1 (object &optional stream)
        (rebind-write (lambda () (prin1 object (output-stream-designator stream)))))
      (print (object &optional stream)
        (rebind-write (lambda () (print object (output-stream-designator stream)))))
      (pprint (object &optional stream)
        (rebind-write (lambda () (pprint object (output-stream-designator stream)))))
      (princ (object &optional stream)
        (rebind-write (lambda () (princ object (output-stream-designator stream)))))
      (write-to-string (object &rest keys &key &allow-other-keys)
        (rebind-write (lambda () (apply #'write-to-string object keys))))
      (prin1-to-string (object &rest keys &key &allow-other-keys)
        (rebind-write (lambda () (apply #'prin1-to-string object keys))))
      (princ-to-string (object &rest keys &key &allow-other-keys)
        (rebind-write (lambda () (apply #'princ-to-string object keys))))
      ;; 23 Reader
      (copy-readtable (&optional (from (creadtable)) to) (copy-readtable from to))
      (make-dispatch-macro-character (char &optional non-terminating-p
                                           (readtable (creadtable)))
        (make-dispatch-macro-character char non-terminating-p readtable))
      (read (&optional stream (eof-error-p t) eof-value recursivep)
        (rebind-read
         (lambda ()
           (read (input-stream-designator stream) eof-error-p eof-value recursivep))))
      (read-preserving-whitespace (&optional stream (eof-error-p t) eof-value recursivep)
        (rebind-read
         (lambda () (read-preserving-whitespace (input-stream-designator stream)
                                                eof-error-p eof-value recursivep))))
      (read-delimited-list (char &optional stream recursivep)
        (rebind-read
         (lambda () (read-delimited-list char (input-stream-designator stream) recursivep))))
      (read-from-string (string &optional (eof-error-p t) eof-value
                                &rest keys &key &allow-other-keys)
        (rebind-read
         (lambda () (apply #'read-from-string string eof-error-p eof-value keys))))
      (set-dispatch-macro-character (disp-char sub-char new-function
                                               &optional (readtable (creadtable)))
        (set-dispatch-macro-character disp-char sub-char new-function readtable))
      (get-dispatch-macro-character (disp-char sub-char &optional (readtable (creadtable)))
        (get-dispatch-macro-character disp-char sub-char readtable))
      (set-macro-character (char function
                                 &optional non-terminating-p (readtable (creadtable)))
        (set-macro-character char function non-terminating-p readtable))
      (get-macro-character (char &optional (readtable (creadtable)))
        (get-macro-character char readtable))
      (set-syntax-from-char (to-char from-char &optional (to-rt (creadtable)) from-rt)
        (set-syntax-from-char to-char from-char to-rt from-rt))
      ;; 24 System Construction
      (provide (module-name)
        (pushnew module-name (^symbol-value '*modules*) :test #'string=))
      ;; 25 Environment
      (describe (object &optional stream)
        (describe object (output-stream-designator stream))))))

(defun class-proper-name (client environment class)
  (let ((name (class-name class)))
    (if (eql (clostrum:find-class client environment name nil) class)
        name
        nil)))

(defun install-generic-environment-accessors (client environment)
  ;; Yikes. But I can't think of a better way to do this without using MOP magic
  ;; that would be fairly arcane (to mimic defmethod, etc.)
  (let ((change-class (make-symbol "CHANGE-CLASS"))
        (make-instance (make-symbol "MAKE-INSTANCE"))
        (make-instances-obsolete (make-symbol "MAKE-INSTANCES-OBSOLETE"))
        (make-load-form (make-symbol "MAKE-LOAD-FORM"))
        (ensure-generic-function-using-class
          (make-symbol "ENSURE-GENERIC-FUNCTION-USING-CLASS"))
        (ensure-class-using-class (make-symbol "ENSURE-CLASS-USING-CLASS")))
    (eval `(defgeneric ,change-class (instance new-class &key &allow-other-keys)
             (:method ((instance standard-object) (new-class standard-class) &rest initargs)
               ;; use the host change-class.
               (apply #'change-class instance new-class initargs))
             (:method ((instance t) (new-class symbol) &rest initargs)
               (apply (function ,change-class) instance
                      ;; we use the client and environment as literals here.
                      (clostrum:find-class ',client ',environment new-class) initargs))))
    (eval `(defgeneric ,make-instance (class &rest initargs &key &allow-other-keys)
             (:method ((class standard-class) &rest initargs)
               (apply #'make-instance class initargs)) ; host make-instance
             (:method ((class symbol) &rest initargs)
               (apply (function ,make-instance)
                      (clostrum:find-class ',client ',environment class)
                      initargs))))
    (eval `(defgeneric ,make-instances-obsolete (class)
             (:method ((class standard-class)) (make-instances-obsolete class))
             (:method ((class symbol))
               (,make-instances-obsolete
                (clostrum:find-class ',client ',environment class)))))
    (eval `(defgeneric ,make-load-form (object &optional env)
             (:method ((object standard-object) &optional env)
               (declare (ignore env))
               (error "No ~s defined for ~s" 'make-load-form object))
             (:method ((object structure-object) &optional env)
               (declare (ignore env))
               (error "No ~s defined for ~s" 'make-load-form object))
             (:method ((object condition) &optional env)
               (declare (ignore env))
               (error "No ~s defined for ~s" 'make-load-form object))
             (:method ((object class) &optional env)
               (let ((name (class-proper-name ',client (or env ',environment) object)))
                 (if name
                     (values `(find-class ',name) nil)
                     (error "~s lacks a proper name" object))))))
    (eval `(defgeneric ,ensure-generic-function-using-class
               (generic-function-or-nil function-name
                &key argument-precedence-order declarations documentation
                  generic-function-class lambda-list method-class method-combination
                  name &allow-other-keys)
             ;; FIXME: Method combination defaulting?
             (:method ((gf generic-function) fname &rest keys
                       &key (method-class 'standard-method)
                         (generic-function-class (class-of gf))
                       &allow-other-keys)
               (setf keys (copy-list keys))
               (remf keys :generic-function-class)
               (remf keys :method-class)
               (etypecase generic-function-class
                 (symbol (setf generic-function-class
                               (clostrum:find-class ',client ',environment
                                                    generic-function-class)))
                 (class))
               (unless (eq generic-function-class (class-of gf))
                 (error "Cannot change the class of generic function ~a from ~a to ~a."
                        fname (class-name (class-of gf))
                        (class-name generic-function-class)))
               (etypecase method-class
                 (symbol (setf method-class
                               (clostrum:find-class ',client ',environment
                                                    method-class)))
                 (class)) ;; initialization checks that it's a subtype of METHOD
               (apply #'reinitialize-instance gf
                      :name fname :method-class method-class keys))
             (:method ((gf null) fname &rest keys
                       &key (method-class 'standard-method)
                         (generic-function-class 'standard-generic-function)
                       &allow-other-keys)
               (setf keys (copy-list keys))
               (remf keys :generic-function-class)
               (remf keys :method-class)
               (etypecase method-class
                 (symbol (setf method-class
                               (clostrum:find-class ',client ',environment
                                                    method-class)))
                 (class))
               (etypecase generic-function-class
                 (symbol (setf generic-function-class
                               (clostrum:find-class ',client ',environment
                                                    generic-function-class)))
                 (class))
               ;; host SUBTYPEP ok since we're giving it explicit classes.
               ;; host generic-function class is also ok as we're assuming this
               ;; environment uses host classes.
               (unless (subtypep generic-function-class
                                 #.(find-class 'generic-function))
                 (error "~s is not a subclass of ~s"
                        (class-name generic-function-class) 'generic-function))
               (setf (clostrum:fdefinition ',client ',environment fname)
                     ;; host make-instance ok, we are passing a class
                     (apply #'make-instance generic-function-class
                            :method-class method-class :name fname keys)))))
    (eval `(flet ((resolve-classes (classes)
                    (loop for c in classes
                          collect (etypecase c
                                    (symbol
                                     (or (clostrum:find-class ',client ',environment
                                                              c nil)
                                       ;; call ECUC directly so that the class
                                       ;; is installed in the right environment.
                                       ;; indirection is to avoid a stupid
                                       ;; function undefinedness warning.
                                       (funcall (fdefinition ',ensure-class-using-class)
                                                nil c :metaclass 'mop:forward-referenced-class)))
                                    (class c))))
                  (resolve-metaclass (class)
                    (etypecase class
                      (symbol (clostrum:find-class ',client ',environment class))
                      (class class))))
             (defgeneric ,ensure-class-using-class
                 (class cname &key direct-default-initargs direct-slots
                                direct-superclasses name metaclass
                  &allow-other-keys)
               (:method ((class class) name &rest keys
                         &key direct-superclasses (metaclass (class-of class))
                         &allow-other-keys)
                 (setf keys (copy-list keys))
                 (remf keys :metaclass)
                 (remf keys :direct-superclasses)
                 (setf direct-superclasses (resolve-classes direct-superclasses))
                 (setf metaclass (resolve-metaclass metaclass))
                 (unless (eq metaclass (class-of class))
                   (error "Cannot change metaclass of ~s" class))
                 (apply #'reinitialize-instance class
                        :direct-superclasses direct-superclasses :name name keys))
               (:method ((class mop:forward-referenced-class) name &rest keys
                         &key direct-superclasses metaclass &allow-other-keys)
                 (setf keys (copy-list keys))
                 (remf keys :metaclass)
                 (remf keys :direct-superclasses)
                 (setf direct-superclasses (resolve-classes direct-superclasses))
                 (setf metaclass (resolve-metaclass metaclass))
                 (apply #'change-class class metaclass
                        :direct-superclasses direct-superclasses :name name keys))
               (:method ((class null) name &rest keys
                         &key direct-superclasses metaclass &allow-other-keys)
                 (setf keys (copy-list keys))
                 (remf keys :metaclass)
                 (remf keys :direct-superclasses)
                 (setf direct-superclasses (resolve-classes direct-superclasses))
                 (setf metaclass (resolve-metaclass metaclass))
                 (setf (clostrum:find-class ',client ',environment name)
                       (apply #'make-instance metaclass
                              :direct-superclasses direct-superclasses
                              :name name keys))))))
    (setf (clostrum:fdefinition client environment 'make-instance) (fdefinition make-instance)
          (clostrum:fdefinition client environment 'change-class) (fdefinition change-class)
          (clostrum:fdefinition client environment 'make-instances-obsolete)
          (fdefinition make-instances-obsolete)
          (clostrum:fdefinition client environment 'make-load-form) (fdefinition make-load-form)
          (clostrum:fdefinition client environment 'mop:ensure-generic-function-using-class)
          (fdefinition ensure-generic-function-using-class)
          (clostrum:fdefinition client environment 'mop:ensure-class-using-class)
          (fdefinition ensure-class-using-class))
    ;; These are ordinary functions, but they rely on the generics.
    (let ((egfuc (fdefinition ensure-generic-function-using-class))
          (ecuc (fdefinition ensure-class-using-class)))
      (defaliases client environment
        (ensure-generic-function (function-name &rest keys)
          (if (clostrum:fboundp client environment function-name)
              (let ((gf (clostrum:fdefinition client environment function-name)))
                (unless (typep gf #.(find-class 'generic-function))
                  (error "~s already names a non-generic function" function-name))
                (apply egfuc gf function-name keys))
              (apply egfuc nil function-name keys)))
        (mop:ensure-class (class-name &rest keys)
          (apply ecuc (clostrum:find-class client environment class-name nil)
                 class-name keys)))))
  nil)

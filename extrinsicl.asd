(defsystem "extrinsicl"
  :description "Extrinsic Common Lisp environment."
  :author ("Bike <aeshtaer@gmail.com>")
  :version "0.1.0"
  :license "BSD"
  :depends-on (#:clostrum #:trucler
                          #:common-macro-definitions ; various
                          #:closer-mop ; make-load-form-saving-slots
                          #:invistra-extrinsic ; format
                          #:khazern-extrinsic) ; loop
  :components
  ((:file "packages")
   (:file "type" :depends-on ("packages"))
   (:file "access" :depends-on ("packages"))
   (:file "defaliases" :depends-on ("packages"))
   (:file "environment-access-functions"
    :depends-on ("defaliases" "access" "type" "packages"))
   (:file "proclaim" :depends-on ("type" "packages"))
   (:file "setf-expansions" :depends-on ("access" "defaliases" "packages"))
   (:file "common-macros" :depends-on ("packages"))
   (:file "loop" :depends-on ("packages"))
   (:file "format" :depends-on ("defaliases" "packages"))
   (:file "simple-aliases" :depends-on ("packages"))
   (:file "condition-system-aliases" :depends-on ("defaliases" "packages"))
   (:file "make-load-form-saving-slots" :depends-on ("defaliases" "packages"))
   (:file "special-operators" :depends-on ("packages"))
   #+(or)
   (:file "compilation-environment" :depends-on ("special-operators" "packages"))
   (:file "variables" :depends-on ("packages"))
   (:file "install" :depends-on ("loop" "simple-aliases" "environment-access-functions"
                                        "setf-expansions" "common-macros"
                                        "special-operators"))))

(defsystem "extrinsicl/maclina"
  :author ("Bike <aeshtaer@gmail.com>")
  :license "BSD"
  :depends-on (#:clostrum #:maclina)
  :components ((:file "maclina")))

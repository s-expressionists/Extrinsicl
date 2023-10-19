(defsystem "extrinsicl"
  :description "Extrinsic Common Lisp environment."
  :author ("Bike <aeshtaer@gmail.com>")
  :version "0.1.0"
  :license "BSD"
  :depends-on (#:clostrum #:trucler
                          #:common-macro-definitions ; various
                          #:khazern-extrinsic) ; loop
  :components
  ((:file "packages")
   (:file "type" :depends-on ("packages"))
   (:file "environment-access-functions" :depends-on ("type" "packages"))
   (:file "setf-expansions" :depends-on ("environment-access-functions" "packages"))
   (:file "common-macros" :depends-on ("packages"))
   (:file "loop" :depends-on ("packages"))
   (:file "simple-aliases" :depends-on ("packages"))
   (:file "special-operators" :depends-on ("packages"))
   (:file "variables" :depends-on ("packages"))
   (:file "install" :depends-on ("loop" "simple-aliases" "environment-access-functions"
                                        "setf-expansions" "common-macros"
                                        "special-operators"))))

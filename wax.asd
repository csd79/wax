(defsystem "wax"
  :description "Scripting environment for MS Office apps"
  :author      "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version     "0.12"
  :depends-on  ("ccom" "cref" "cl-ppcre" "local-time")
  :serial      t
  :components  ((:file "package")
                (:file "fli-templates")
                (:file "utilities")
                (:file "wax")
                (:file "script")))

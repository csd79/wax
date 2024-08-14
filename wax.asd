(defsystem "wax"
  :description "Scripting environment for MS Office apps"
  :author      "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version     "0.07"
  :depends-on  ("ccom" "cl-ppcre")
  :serial      t
  :components  ((:file "package")
                (:file "fli-templates")
                (:file "utilities")
                (:file "wax")))

(defsystem "wax"
  :description "Scripting environment for MS Office apps"
  :author      "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version     "0.21"
  :depends-on  ("ccom" "cref" "achar" "cl-ppcre" "local-time" "str")
  :serial      t
  :components  ((:file "package")
                (:file "fli-templates")
                (:file "utilities")
                (:file "gui")
                (:file "wax")
                (:file "script")))

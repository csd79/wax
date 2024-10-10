(defsystem "wax"
  :description "Scripting environment for MS Office apps"
  :author      "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version     "0.27"
  :depends-on  ((:version "ccom" "0.18") "cref" "achar" "cl-ppcre" "local-time" "str")
  :serial      t
  :components  ((:file "package")
                (:file "fli-templates")
                (:file "utilities")
                (:file "gui")
                (:file "wax")
                (:file "script")))

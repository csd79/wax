(defsystem "clex"
  :description "Punishing Excel"
  :author      "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version     "0.01"
  :depends-on  ("comwrapper")
  :serial      t
  :components  ((:file "package")
                (:file "fli-templates")
                (:file "clex")))

(defsystem "wax"
  :description "Interpolate stuff into MS Word and Excel documents"
  :author      "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version     "0.03"
  :depends-on  ("ccom" "cl-ppcre")
  :serial      t
  :components  ((:file "package")
                (:file "fli-templates")
                (:file "utilities")
                (:file "wax")))

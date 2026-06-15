(defsystem "wax"
  :description "Scripting environment for MS Office apps"
  :author      "Denes Cselovszki <denes.cselovszky@gmail.com>"
  :version     "0.46"
  :depends-on  ("cl-ppcre" "local-time" "str" "achar" "ccom4" "ccoffice" "cref")
  :serial      t
  :components  ((:file "package")
;                (:file "fli-templates")
                (:file "utilities")
                (:file "gui")
                (:file "error-handling")
                (:file "wax")
;                (:file "script")
				))

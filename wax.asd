(defsystem "wax"
  :description "Scripting environment for MS Office apps"
  :author      "Denes Cselovszki <denes.cselovszky@gmail.com>"
  :version     "0.39"
;  :depends-on  ("ccom3" "msoffice" "cref" "achar" "cl-ppcre" "local-time" "str") ; leforduljon
  :depends-on  ("cl-ppcre" "local-time" "str" "achar" "ccom3" "msoffice" "cref") ; menjen unicode szövegek olvasása excelből?
  :serial      t
  :components  ((:file "package")
                (:file "fli-templates")
                (:file "utilities")
                (:file "gui")
                (:file "error-handling")
                (:file "wax")
                (:file "script")))

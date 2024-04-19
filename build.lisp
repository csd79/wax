;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 

(in-package "CL-USER")

(load "c:\\Users\\cselovszkid\\.lispworks")

(asdf:load-system "clex")

(setf snewper::*independent-exe* t)
(lw:deliver 'clex:start
    "c:\\Users\\cselovszkid\\common-lisp\\clex\\clex.exe"
    1
    :interface :capi
    :console :io
    :multiprocessing t
    :icon-file "c:\\Users\\cselovszkid\\common-lisp\\clex\\clex.ico"
    :error-on-interpreted-functions t
    :keep-eval t
    :keep-lisp-reader t
    :symbol-names-action nil
    :startup-bitmap-file nil
    )

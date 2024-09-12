;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 

(in-package "CL-USER")

(load "c:\\Users\\cselovszkid\\.lispworks")

(asdf:load-system "wax")

(lw:deliver 'wax:start
    "c:\\Users\\cselovszkid\\common-lisp\\wax\\wax_v0.17.exe"
    5
    :interface :capi
    :console :io
    :multiprocessing t
    :icon-file "c:\\Users\\cselovszkid\\common-lisp\\wax\\img\\wax.ico"
    :error-on-interpreted-functions t
    :keep-eval t
    :keep-lisp-reader t
    :symbol-names-action nil
    :startup-bitmap-file "c:\\Users\\cselovszkid\\common-lisp\\wax\\img\\waxman_dither.bmp"
    :kill-dspec-table nil)

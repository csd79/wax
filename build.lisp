;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 

(in-package "CL-USER")
(load "c:\\Users\\cselovszkid\\.lispworks")
;(load "c:\\Users\\csd79\\.lispworks")
(asdf:load-system "wax")

(in-package "WAX")
(setf *independent-exe* t)
(lw:deliver 'start
    "c:\\Users\\cselovszkid\\common-lisp\\wax\\wax_v0.38.exe"
;    "c:\\Users\\csd79\\common-lisp\\wax\\wax_v0.38.exe"
    5
    :interface :capi
    :console :io
    :multiprocessing t
    :icon-file "c:\\Users\\cselovszkid\\common-lisp\\wax\\img\\wax.ico"
;    :icon-file "c:\\Users\\csd79\\common-lisp\\wax\\img\\wax.ico"
    :keep-package-manipulation t
    :keep-function-name :all
    :keep-eval t
    :keep-lisp-reader t
    :symbol-names-action nil
    :startup-bitmap-file nil
    :kill-dspec-table nil
    :keep-conditions :all
    :keep-debug-mode t
    :keep-load-function t
    :compact t
    )

;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;; ----------------------------------------------------------------------
;; Detail extractors for condition classes


;;; Extract details of COM-DISPATCH-INVOKE-EXCEPTION-ERROR into a plist.
(defun com-dispatch-invoke-exception-error-details (condition)
  (with-slots ((fn-name     com::function-name)
               (hresult     com::hresult)
               (method-name com::method-name)
               (reporter-fn conditions::reporter-function))
      condition
    (destructuring-bind (code source description help-file help-context)
        (com:com-dispatch-invoke-exception-error-info
         condition
         '(:code :source :description :help-file :help-context))
      (list :fn-name      fn-name
            :hresult      hresult
            :method-name  method-name
            :reporter-fn  reporter-fn
            :code         code
            :source       source
            :description  description
            :help-file    help-file
            :help-context help-context))))


;;; Extract details of COM-ERROR into a plist.
(defun com-error-details (condition)
  (list :hresult (com::com-error-hresult condition)
        :fn-name (com::com-error-function-name condition)))


;;; For general errors & conditions, give the condition message or type.
(defun condition-string-or-type (condition)
  (list :data (typecase condition
                (string condition)
                (t (type-of condition)))))


;;; Return the current backtrace as a string.
(defun backtrace->string ()
  (with-output-to-string (out)
    (dbg:output-backtrace :bug-form :stream out)))
#|  (let ((out (make-string-output-stream)))
    (dbg:output-backtrace :bug-form :stream out)
    (get-output-stream-string out)))|#


;; ----------------------------------------------------------------------
;; Handling errors


;;; Call WF-ERRORDIAL with generated errod message and details.
(defun dispatch-wg-errordial (details control-string &rest keys)
  (let ((vals (when details
                (mapcar #'(lambda (key)
                            (getf details key)) keys))))
    (wg-errordial (apply #'format nil control-string vals)
                  (append details
                          (list :backtrace
                                (backtrace->string))))))


;;; Wrapper macro to add errorhandling to main loop.
(defmacro with-wax-errorsink (&body body)
  `(if *independent-exe*
     ;; Errorsink on
     (catch 'sinked
       (handler-bind ((com:com-dispatch-invoke-exception-error
                       #'(lambda (error)
                           (dispatch-wg-errordial
                            (com-dispatch-invoke-exception-error-details error)
                            "~a: ~a: ~a" :source :method-name :description)
                           (throw 'sinked nil)))
                      (com:com-error
                       #'(lambda (error)
                           (dispatch-wg-errordial
                            (com-error-details error)
                            "Hiba: hresult: ~a; függvény: ~a" :hresult :fn-name)
                           (throw 'sinked nil)))
                      (error
                       #'(lambda (error)
                           (dispatch-wg-errordial
                            (condition-string-or-type error)
                            "Hiba: ~a" :data)
                           (throw 'sinked nil)))
                      (condition
                       #'(lambda (condition)
                           (dispatch-wg-errordial
                            (condition-string-or-type condition)
                            "Váratlan állapot: ~a" :data)
                           (throw 'sinked nil))))
         ,@body))
     ;; Conditions passed to LW.
     (progn ,@body)))


;; ----------------------------------------------------------------------
;; Sandbox


(defun b ()
  (with-wax-errorsink
    (with-document (:doc doc :open "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\Kinevezések\\Pedagógus_kinevezési okmány_xxx.docx" :read-only t)
      (cclet* ((content #~('content doc))
               (text    #~('text content)))
        (format t "~a~%~%" text)))))


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
  (let ((out (make-string-output-stream)))
    (dbg:output-backtrace :bug-form :stream out)
    (get-output-stream-string out)))


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
(defmacro with-wax-errorhandling (&body body)
  `(handler-case
       (progn
         ,@body)
     (com:com-dispatch-invoke-exception-error (e)
       (dispatch-wg-errordial (com-dispatch-invoke-exception-error-details e)
                              "~a: ~a: ~a" :source :method-name :description))
     (com:com-error (e)
       (dispatch-wg-errordial (com-error-details e)
                              "Hiba: hresult: ~a; függvény: ~a" :hresult :fn-name))
     (error (e)
       (dispatch-wg-errordial (condition-string-or-type e)
                              "Hiba: ~a" :data))
     (condition (c)
       (dispatch-wg-errordial (condition-string-or-type c)
                              "Váratlan állapot: ~a" :data))))


;; ----------------------------------------------------------------------
;; Sandbox


(defun b ()
  (with-wax-errorhandling
    (with-document (:doc doc :open "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\Kinevezések\\Pedagógus_kinevezési okmány_xxx.docx" :read-only t)
      (cclet* ((content #~('content doc))
               (text    #~('text content)))
        (format t "~a~%~%" text)))))


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
(defun condition-string (condition)
  (list :data (typecase condition
                (string           condition)
                (arithmetic-error   (format nil "Aritmetikai hiba. Függvény: \"~a\", paraméterek: ~a."
                                            (arithmetic-error-operation condition)
                                            (arithmetic-error-operands condition)))
                (file-error         (format nil "Fájl hiba. Elérési út: ~a."
                                            (file-error-pathname condition)))
                (type-error         (format nil "Típushiba. Hibás adat: ~a, kívánt típus: ~a."
                                            (type-error-datum condition)
                                            (type-error-expected-type condition)))
                (unbound-variable   (format nil "Kötetlen változó. Változónév: ~a."
                                            (cell-error-name condition)))
                (undefined-function (format nil "Nemdefiniált függvény. Függvénynév: ~a."
                                            (cell-error-name condition)))
                (unbound-slot       (format nil "Kötetlen slot. Slot neve: ~a."
                                            (cell-error-name condition)))
                (t                  (type-of condition)))))


;;; Return the current backtrace as a string.
(defun backtrace->string (&optional (level 0))
  (with-output-to-string (out)
    (format out "Backtrace generated at: ~a~2%" (timestamp (get-universal-time)))
    (dbg:output-backtrace
     (case level
       (0 :quick)
       (1 :brief)
       (2 :verbose)
       (3 :bug-form)
       (otherwise :quick))
     :stream out)))


;; ----------------------------------------------------------------------
;; Handling errors


(define-condition wax-skipped (condition)
  ((errorlog :accessor errorlog :initarg :errorlog)
   (errordump :accessor errordump :initarg :errordump)
   (throw-point :accessor throw-point :initarg :skip-to)))


(defmacro defmessenger (fname ((var) &rest args) ctrl-string &rest params)
  `(defun ,fname ,args
     #'(lambda (,var)
         (format nil ,ctrl-string ,@params))))


(defparameter *noskip-classes* '(wax-skipped undefined-function)) ; kludge exception for CCOM-ACCESSORS


(defun skippable-handler (messenger skip-to)
  #'(lambda (condition)
      (unless (member (type-of condition) *noskip-classes*)
        (error (make-condition
                'wax-skipped
                :errorlog (funcall messenger
                                   (getf (condition-string condition) :data))
                :errordump (backtrace->string 2)
                :skip-to skip-to)))))


(defmacro skippable ((class skip-to messenger) &body body)
  "Meant to be used inside WITH-WAX-ERRORSINK. If condition of class CLASS happens inside BODY and CLASS is not a member of *NOSKIP-CLASSES*, throw a WAX-SKIPPED condition which will be caught by WITH-WAX-ERRORSINK and cause the string returned by MESSENGER to be added to the progress windows message queues, and then exit to SKIP-TO."
  `(handler-bind ((,class (skippable-handler ,messenger ,skip-to)))
     ,@body))


(defparameter *errorsink-on* nil "Switch errorsink on/off.")

(defun errorsink-on ()
  *errorsink-on*)

(defun (setf errorsink-on) (value)
  (setf *errorsink-on* (not (not value))))


#|(defun dispatch-wg-errordial (details control-string &rest keys)
  "Call WF-ERRORDIAL with generated error message and details."
  (let ((vals (when details
                (mapcar #'(lambda (key)
                            (getf details key)) keys))))
    (wg-errordial (apply #'format nil control-string vals)
                  (append details
                          (list :backtrace
                                (backtrace->string 2))))))|#


(defun construct-errorlog (condition details-fn ctrl-string keys)
  "Construct message to be stored in the errorlog queue."
  (let* ((details (funcall details-fn condition))
         (values  (mapcar #'(lambda (key) (getf details key)) keys)))
    (if details
      (apply #'format nil ctrl-string values)
      "")))


(defun kill-n-sink (obj details-fn ctrl-string &rest keys)
  "Handler for an unskippable condition."
  (lambda (condition)
    (when *errorsink-on*
      (pkill obj)
      (queue-errorlog obj (construct-errorlog condition details-fn ctrl-string keys))
      (queue-errordump obj (backtrace->string 2))
      (wg-errordial obj)) ;     <-------------------------------------------------------------!!!!!!!!
      (throw 'sinked nil)))


(defun skip (obj)
  "Handler for a skippable condition,"
  (lambda (condition)
    (queue-errorlog obj (errorlog condition))
    (queue-errordump obj (errordump condition))
    (throw (throw-point condition) nil)))


;;; Wrapper macro to add errorhandling to main loop.
(defmacro with-wax-errorsink (obj &body body)
  `(catch 'sinked
     (handler-bind ((com:com-dispatch-invoke-exception-error
                     (kill-n-sink ,obj #'com-dispatch-invoke-exception-error-details
                                  "~a: ~a: ~a" :source :method-name :description))
                    (com:com-error
                     (kill-n-sink ,obj #'com-error-details
                                  "Hiba: hresult: ~a; függvény: ~a" :hresult :fn-name))
                    (error
                     (kill-n-sink ,obj #'condition-string "Hiba: ~a" :data))
                    (wax-skipped
                     (skip ,obj))
                    (condition
                     (kill-n-sink ,obj #'condition-string "Váratlan állapot: ~a" :data)))
       ,@body)))
#|(defmacro with-wax-errorsink (obj &body body)
  `(catch 'sinked
     (handler-bind ((com:com-dispatch-invoke-exception-error
                     #'(lambda (error)
                         (when *errorsink-on*
                           (pkill ,obj)
                           (dispatch-wg-errordial
                            (com-dispatch-invoke-exception-error-details error)
                            "~a: ~a: ~a" :source :method-name :description)
                           (throw 'sinked nil))))
                    (com:com-error
                     #'(lambda (error)
                         (when *errorsink-on*
                           (pkill ,obj)
                           (dispatch-wg-errordial
                            (com-error-details error)
                            "Hiba: hresult: ~a; függvény: ~a" :hresult :fn-name)
                           (throw 'sinked nil))))
                    (error
                     #'(lambda (error)
                         (when *errorsink-on*
                           (pkill ,obj)
                           (dispatch-wg-errordial
                            (condition-string error)
                            "Hiba: ~a" :data)
                           (throw 'sinked nil))))
                    (wax-skipped
                     #'(lambda (condition)
                         (queue-errorlog obj (errorlog condition))
                         (queue-errordump obj (errordump condition))
                         (throw (throw-point condition) nil)))
                    (condition
                     #'(lambda (condition)
                         (when *errorsink-on*
                           (pkill ,obj)
                           (dispatch-wg-errordial
                            (condition-string condition)
                            "Váratlan állapot: ~a" :data)
                           (throw 'sinked nil)))))
       ,@body)))|#



;; ----------------------------------------------------------------------
;; Sandbox


(defun b ()
  (with-wax-errorsink
    (with-document (:doc doc :open "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\Kinevezések\\Pedagógus_kinevezési okmány_xxx.docx" :read-only t)
      (cclet* ((content (?content doc))
               (text    (?text content)))
        (format t "~a~%~%" text)))))


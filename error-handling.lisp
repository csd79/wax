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


#|;;; For general errors & conditions, give the condition message or type.
(defun condition-string (condition)
  (list :data (typecase condition
                (string condition)
                (t (type-of condition)))))|#


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
(defun backtrace->string ()
  (with-output-to-string (out)
    (dbg:output-backtrace

     :quick
;     :brief
;     :verbose
;     :bug-form

     :stream out)))


;; ----------------------------------------------------------------------
;; Handling errors


(define-condition wax-skippable (condition)
  ((message :accessor message :initarg :message)
   (throw-point :accessor throw-point :initarg :skip-to)))


(defmacro defmessenger (fname ((var) &rest args) ctrl-string &rest params)
  `(defun ,fname ,args
     #'(lambda (,var)
         (format nil ,ctrl-string ,@params))))


(defparameter *noskip-classes* '(wax-skippable undefined-function)) ; kludge exception for CCOM-ACCESSORS


(defun skippable-handler (messenger skip-to)
  #'(lambda (condition)
;      (wg-msg "~a  ~a~%~a" (type-of condition) skip-to condition)
      (unless (member (type-of condition) *noskip-classes*)
        (error (make-condition
                'wax-skippable
                :message (funcall messenger
                                  (getf (condition-string condition) :data))
                :skip-to skip-to)))))


(defmacro skippable ((class skip-to messenger) &body body)
  "Meant to be used inside WITH-WAX-ERRORSINK. If condition of class CLASS happens inside BODY and CLASS is not a member of *NOSKIP-CLASSES*, throw a WAX-SKIPPABLE condition which will be caught by WITH-WAX-ERRORSINK and cause the value of MESSENGER to be added to the progress windows message queue, and then exit to SKIP-TO."
  `(handler-bind ((,class (skippable-handler ,messenger ,skip-to)))
     ,@body))


#|(defun tc ()
  (handler-bind ((wax-skippable
                  #'(lambda (condition)
                      (print (message condition))
                      (throw (throw-point condition) nil))))
    (skippable (condition :fuuk #'(lambda (cnd)
                                    (format nil "~a hihi ~%" cnd)))
      (catch 'fuuk
;        (print (/ 7 0))
        (print (boogaloo 1 2))
        (print (/ 7 0))))))|#





;;; Call WF-ERRORDIAL with generated errod message and details.
(defun dispatch-wg-errordial (details control-string &rest keys)
  (let ((vals (when details
                (mapcar #'(lambda (key)
                            (getf details key)) keys))))
    (wg-errordial (apply #'format nil control-string vals)
                  (append details
                          (list :backtrace
                                (backtrace->string))))))


(defparameter *errorsink-on* nil "Switch errorsink on/off.")

(defun errorsink-on ()
  *errorsink-on*)

(defun (setf errorsink-on) (value)
  (setf *errorsink-on* (not (not value))))


;;; Wrapper macro to add errorhandling to main loop.
(defmacro with-wax-errorsink (obj &body body)
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
                    (wax-skippable
                     #'(lambda (condition)
                         (queue-error-message obj (message condition))
                         (throw (throw-point condition) nil)))
                    (condition
                     #'(lambda (condition)
                         (when *errorsink-on*
                           (pkill ,obj)
                           (dispatch-wg-errordial
                            (condition-string condition)
                            "Váratlan állapot: ~a" :data)
                           (throw 'sinked nil)))))
       ,@body)))

                           



;; ----------------------------------------------------------------------
;; Sandbox


(defun b ()
  (with-wax-errorsink
    (with-document (:doc doc :open "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\Kinevezések\\Pedagógus_kinevezési okmány_xxx.docx" :read-only t)
      (cclet* ((content (?content doc))
               (text    (?text content)))
        (format t "~a~%~%" text)))))


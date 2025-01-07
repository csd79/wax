;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;; ----------------------------------------------------------------------
;; Script state

#|(defparameter *state-file* "state" "Textfile to store script state between runs.")

(defclass wax-script ()
  ((state
    :initarg :state
    :accessor state)
   (execute-fn
    :initarg :execute-fn
    :accessor execute-fn)
   (dump-fn
    :initarg :dump-fn
    :accessor dump-fn)
   (pstep-limit
    :initarg :pstep-limit
    :accessor pstep-limit)
   (pstep-fn
    :initarg :pstep-fn
    :accessor pstep-fn)
   (pabort-fn
    :initarg :pabort-fn
    :accessor pabort-fn))
  (:documentation "'Global' state of a wax script."))|#


;; ----------------------------------------------------------------------
;; Progress window methods

#|(defmethod dump ((obj wax-script) control-string &rest args)
  (apply (dump-fn obj) control-string args))

(defmethod set-pstep-limit ((obj wax-script) limit)
  (setf (pstep-limit obj) limit))

(defmethod pstep ((obj wax-script) &key (abs nil) (step 1))
  (funcall (pstep-fn obj) :abs abs :step step))

(defmethod pabort ((obj wax-script))
  (funcall (pabort-fn obj)))|#


;; ----------------------------------------------------------------------
;; State permanency

#|(defmethod save-state ((obj wax-script))
  (save-forms
   (appfile *state-file*)
   (state obj)))

(defmethod load-state ((obj wax-script))
  (setf (state obj)
        (load-forms (appfile *state-file*))))

(defmethod init-state ((obj wax-script) &rest plist)
  (setf (state obj) plist))|#


;; ----------------------------------------------------------------------
;; Execution

#|(defmethod set-execute-fn ((obj wax-script) fn)
  (setf (efecute-fn obj) fn))

(defmethod init-wax-script ((obj wax-script) &key
                            (state '())
                            (errorsink-on nil)
                            (prop-accessors-on t)
                            (execute-fn nil))
  (init-state obj state)
  (setf (errorsink-on) errorsink-on)
  (setf (property-accessors-on) prop-accessors-on)
  (set-execute-fn obj execute-fn))

(defmethod wax-execute ((obj wax-script))
  (with-wax-errorsink
      (with-property-accessors 
        (funcall (execute-fn obj) obj))))|#

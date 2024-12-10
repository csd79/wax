;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;; ----------------------------------------------------------------------
;; Script state


(defparameter *state-file* "state" "Textfile to store script state between runs.")


(defclass wax-script ()
  ((state
    :initarg :state
    :accessor state)
   (errorsink-on-p
    :initarg :errorsink-on-p
    :accessor errorsink-on-p)
   (prop-accessors-on-p
    :initarg :prop-accessors-on-p
    :accessor prop-accessors-on-p)
   (execute-fn
    :initarg :execute-fn
    :accessor execute-fn)
   (dump-fn
    :initarg :dump-fn
    :accessor dump-fn)
   (pstep-fn
    :initarg :pstep-fn
    :accessor pstep-fn)
   (pabort-fn
    :initarg :pabort-fn
    :accessor pabort-fn))
  (:documentation "'Global' state of a wax script."))

(defmethod dump ((obj wax-script) control-string &rest args)
  (apply (dump-fn obj) control-string args))

(defmethod pstep ((obj wax-script) &key (abs nil) (step 1))
  (funcall (pstep-fn obj) :abs abs :step step))

(defmethod pabort ((obj wax-script))
  (funcall (pabort-fn obj)))

(defmethod save-state ((obj wax-script))
  (save-forms
   (appfile *state-file*)
   (state obj)))

(defmethod load-state ((obj wax-script))
  (setf (state obj)
        (load-forms (appfile *state-file*))))

(defmethod init-state ((obj wax-script) &rest plist)
  (setf (state obj) plist))


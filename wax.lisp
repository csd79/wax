;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;; ======================================================================
;; Data sources class

(defclass data-source ()
  ((filename
    :initarg :filename
    :accessor filename)
   (data
    :accessor data
    :initform nil)
   (loaded-p
    :accessor loaded-p
    :initform nil))
  (:documentation "Data sources used during app execution."))

(defmethod load-src ((obj data-source) &optional (header-height 1))
  (with-slots ((filename filename)) obj
    (when (and filename
               (string/= filename "")
               (probe-file filename))
      (setf (data obj)
            (with-workbook (:open filename :read-only t :wsvars (wsheet) :close t)
              (read-xarray (used-range wsheet) :from-row (1+ header-height)))
            (loaded-p obj) t)
      )))

(defmethod purge ((obj data-source))
  (setf (data obj) nil
        (loaded-p obj) nil))

(defmethod select-row ((obj data-source) selector-fn)
  (when (data obj)
    (if (loaded-p obj)
      (xaselect (data obj) selector-fn)
      (error "Data source ~a not loaded." (filename obj)))))


;; ======================================================================
;; App state class

(defparameter *state-file* "state" "Textfile to store app state between runs.")

(defclass wax-app ()
  ((state
    :initarg :state
    :accessor state)
   (execute-fn
    :initarg :execute-fn
    :accessor execute-fn)
   (disp-fn
    :accessor disp-fn)
   (pstep-limit
    :accessor pstep-limit)
   (pstep-fn
    :accessor pstep-fn)
   (pabort-fn
    :accessor pabort-fn)
   (pkill-fn
    :accessor pkill-fn
    :initform #'(lambda (&optional obj)
                  (declare (ignore obj))))
   (data-sources
    :accessor data-sources
    :initform '())
   (errorlogs
    :accessor errorlogs
    :initform '())
   (errordumps
    :accessor errordumps
    :initform '()))
  (:documentation "Wax app environment."))


;; ----------------------------------------------------------------------
;; Progress window methods

(defmethod disp ((obj wax-app) control-string &rest args)
  (apply (disp-fn obj) control-string args))

;(defmethod set-pstep-limit ((obj wax-app) limit)
;  (setf (pstep-limit obj) limit))

(defmethod pstep ((obj wax-app) &key (abs nil) (step 1))
  (funcall (pstep-fn obj) :abs abs :step step))

(defmethod pabort ((obj wax-app))
  "Stop the progress loop by user intent."
  (funcall (pabort-fn obj)))

(defmethod pkill ((obj wax-app))
  "Stop the progress loop from a WITH-WAX-ERRORSINK clause."
  (funcall (pkill-fn obj)))


;; ----------------------------------------------------------------------
;; State permanency & handling

(defmethod save-state ((obj wax-app) &key (package-name "WAX") (keys '() keys-provided-p))
  (let* ((filename (appfile *state-file* package-name))
         (state    (state obj))
         (plist    (if keys-provided-p
                     (keep-pairs state keys)
                     state)))
    (when (probe-file filename)
      (hide-file filename nil))
    (save-forms filename plist)))

(defmethod load-state ((obj wax-app) &key (package-name "WAX") (keys '() keys-provided-p))
  (let ((filename (appfile *state-file* package-name))
        (news     '()))
    (when (probe-file filename)
      (hide-file filename nil)
      (setf news (load-forms filename))
      (setf (state obj)
            (if keys-provided-p
              (override-pairs (state obj) (keep-pairs news keys))
              news))
      (hide-file filename t))))

(defmethod init-state ((obj wax-app) &rest plist)
  (setf (state obj) plist))

(defmethod get-state ((obj wax-app) key)
  (getf (state obj) key))

(defun (setf get-state) (value obj key)
  (setf (getf (state obj) key) value))


;; ----------------------------------------------------------------------
;; Handling data sources

(defmethod add-data-source ((obj wax-app) key filename)
  (setf (data-sources obj)
        (override-pairs
         (data-sources obj)
         (list key (make-instance 'data-source :filename filename)))))

(defmethod remove-data-source ((obj wax-app) key)
  (setf (data-sources obj)
        (remove-pairs (data-sources obj) (list key))))

(defmethod load-data-source ((obj wax-app) key &optional (header-height 1))
  (load-src (getf (data-sources obj) key) header-height))

(defmethod purge-data-source ((obj wax-app) key)
  (purge (getf (data-sources obj) key)))

(defmethod select-row-from ((obj wax-app) key selector-fn)
  (let ((data-source (getf (data-sources obj) key)))
    (when (loaded-p data-source)
      (select-row data-source selector-fn))))

(defmethod source-filename ((obj wax-app) key)
  (filename (getf (data-sources obj) key)))

(defmethod source-data ((obj wax-app) key)
  (data (getf (data-sources obj) key)))


;; ----------------------------------------------------------------------
;; Error messages


;; General
(defmethod queue-message ((obj wax-app) getter setter string)
  (funcall setter (cons string (funcall getter obj)) obj))

(defmethod messages-waiting-p ((obj wax-app) getter)
  (not (zerop (length (funcall getter obj)))))

(defmethod disp-messages ((obj wax-app) getter)
  (dolist (message (reverse (funcall getter obj)))
    (disp obj message))
  (disp obj "~6%"))

(defmethod purge-messages ((obj wax-app) setter)
  (funcall setter '() obj))
;  (setf (funcall accessor obj) '()))


;; Errorlogs
(defmethod queue-errorlog ((obj wax-app) string)
  (queue-message obj #'errorlogs #'(setf errorlogs) string))

(defmethod errorlogs-waiting-p ((obj wax-app))
  (messages-waiting-p obj #'errorlogs))

(defmethod disp-errorlogs ((obj wax-app))
  (disp-messages obj #'errorlogs))

(defmethod purge-errorlogs ((obj wax-app))
  (purge-messages obj #'(setf errorlogs)))


;; Errordumps
(defmethod queue-errordump ((obj wax-app) string)
  (queue-message obj #'errordumps #'(setf errordumps) string))

(defmethod errordumps-waiting-p ((obj wax-app))
  (messages-waiting-p obj #'errordumps))

(defmethod disp-errordumps ((obj wax-app))
  (disp-messages obj #'errordumps))

(defmethod purge-errordumps ((obj wax-app))
  (purge-messages obj #'(setf errordumps)))


;; Dump both streams into a single string.
(defmethod fulldump ((obj wax-app))
  (when (errorlogs-waiting-p obj)
    (let ((fd (make-string-output-stream))
          (big-sep (format nil "~a~%~a~%~a~%~a~%~a"
                           (line 77 #\=) (line 77 #\=) (line 77 #\=) (line 77 #\=) (line 77 #\=)))
          (small-sep (line 70)))
      (loop for log in (errorlogs obj)
            for dump in (errordumps obj)
            doing
            (format fd "~a~4%~a~%  HIBAÜZENET:~%~a~4%~a~4%~a~%  BACKTRACE:~%~a~4%~a~8%"
                    big-sep
                    small-sep
                    small-sep
                    log
                    small-sep
                    small-sep
                    dump))
      (get-output-stream-string fd))))


;; ----------------------------------------------------------------------
;; Execution

(defmethod set-execute-fn ((obj wax-app) fn)
  (setf (execute-fn obj) fn))

(defmethod wax-execute ((obj wax-app) &key (errorsink-on nil))
  "Start the function stored in the EXECUTE-FN slot of OBJ with the errorsink active or not."
  (with-wax-errorsink obj
    (setf (errorsink-on) errorsink-on)
    (funcall (execute-fn obj) obj)))


;; ----------------------------------------------------------------------
;; Sandbox



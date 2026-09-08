;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)
#.(enable-ccom-syntax)


;; ======================================================================
;; Data sources class

(defclass data-source ()
  ((filename
    :initarg :filename
    :accessor filename)
   (worksheet
    :initarg :worksheet
    :accessor worksheet)
   (data
    :accessor data
    :initform nil)
   (loaded-p
    :accessor loaded-p
    :initform nil))
  (:default-initargs
   :worksheet 1)
  (:documentation "Data sources used during app execution."))

#|(defmethod load-src ((obj data-source) &key (first-row 2) (header-row (1- first-row)))
  (with-slots ((filename filename)) obj
    (when (and filename
               (string/= filename "")
               (probe-file filename))
      (setf (data obj)
            (with-workbook (:open filename :read-only t :wsvars (wsheet) :close t)
              (read-xarray (used-range wsheet) :from-row first-row :header-row header-row))
            (loaded-p obj) t))))|#
;(defmethod load-src ((obj data-source) &optional (header-height 1))
(defmethod load-src ((obj data-source) &key (first-row 2) (header-row (1- first-row)))
  (with-slots ((filename filename)) obj
    (when (and filename
               (string/= filename "")
               (probe-file filename))
      (setf (data obj)
            (with-workbook (:open filename :wbook wbook :read-only t :wsvars (1st-ws) :close t)
              (cclet* ((wsheets (?'worksheets wbook))
                       (wsheet  (!'item wsheets (worksheet obj))))
                (read-xarray (used-range wsheet) :from-row first-row :header-row header-row)))
            (loaded-p obj) t))))

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
   (errorsink-enabled-p
    :initarg :errorsink-enabled-p
    :accessor errorsink-enabled-p)
   (execute-fn
    :initarg :execute-fn
    :accessor execute-fn)
   (kill-fn
    :accessor kill-fn
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


(defmethod kill ((obj wax-app))
  "Kill the wax app loop from an ERRORSINK clause. Usable when PKILL-FN is initialized."
  (funcall (kill-fn obj) (errorlogs-pending-p obj)))


;; ----------------------------------------------------------------------
;; State permanency & handling


(defmethod save-state ((obj wax-app) &key (package-name nil) (keys '() keys-provided-p))
  (let* ((filename (appfile *state-file* package-name))
         (state    (state obj))
         (plist    (if keys-provided-p
                     (keep-pairs state keys)
                     state)))
    (when (probe-file filename)
      (hide-file filename nil))
    (save-forms filename plist)))

(defmethod load-state ((obj wax-app) &key (package-name nil) (keys '() keys-provided-p))
  (let ((filename (appfile *state-file* package-name))
        (news     '()))
    (when (probe-file filename)
      (hide-file filename nil)
      (setf news (first (load-forms filename)))
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

(defmethod trim-state ((obj wax-app) key)
  (remf (state obj) key))

(defmethod load-descriptives ((obj wax-app) file &rest keys)
  (let* ((length (length keys))
         (forms  (load-forms file :count length)))
    (loop for key in keys
          for form in forms doing
          (setf (get-state obj key) form))))


;; ----------------------------------------------------------------------
;; Handling data sources

#|(defmethod add-data-source ((obj wax-app) key filename)
  (setf (data-sources obj)
        (override-pairs
         (data-sources obj)
         (list key (make-instance 'data-source :filename filename)))))|#
(defmethod add-data-source ((obj wax-app) key filename &optional (worksheet 1))
  (setf (data-sources obj)
        (override-pairs
         (data-sources obj)
         (list key (make-instance 'data-source
                                  :filename  filename
                                  :worksheet worksheet)))))

(defmethod remove-data-source ((obj wax-app) key)
  (setf (data-sources obj)
        (remove-pairs (data-sources obj) (list key))))

(defmethod load-data-source ((obj wax-app) key &key (first-row 2) (header-row (1- first-row)))
  (load-src (getf (data-sources obj) key) :first-row first-row :header-row header-row))

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

(defmethod messages-pending-p ((obj wax-app) getter)
  (not (zerop (length (funcall getter obj)))))

(defmethod display-messages ((obj wax-app) getter stream)
  (dolist (message (reverse (funcall getter obj)))
    (format stream message))
  (format stream "~6%"))

(defmethod purge-messages ((obj wax-app) setter)
  (funcall setter '() obj))


;; Errorlogs
(defmethod queue-errorlog ((obj wax-app) string)
  (queue-message obj #'errorlogs #'(setf errorlogs) string))

(defmethod errorlogs-pending-p ((obj wax-app))
  (messages-pending-p obj #'errorlogs))

(defmethod display-errorlogs ((obj wax-app) stream)
  (display-messages obj #'errorlogs stream))

(defmethod purge-errorlogs ((obj wax-app))
  (purge-messages obj #'(setf errorlogs)))


;; Errordumps
(defmethod queue-errordump ((obj wax-app) string)
  (queue-message obj #'errordumps #'(setf errordumps) string))

(defmethod errordumps-pending-p ((obj wax-app))
  (messages-pending-p obj #'errordumps))

(defmethod display-errordumps ((obj wax-app) stream)
  (display-messages obj #'errordumps stream))

(defmethod purge-errordumps ((obj wax-app))
  (purge-messages obj #'(setf errordumps)))


;; Dump both streams into a single string.
(defmethod full-dump ((obj wax-app))
  (when (errorlogs-pending-p obj)
    (with-slots (errorlogs errordumps) obj
      (let ((fd (make-string-output-stream))
            (big-sep (format nil "~a~%~a~%~a~%~a~%~a"
                             (line 77 #\=) (line 77 #\=) (line 77 #\=) (line 77 #\=) (line 77 #\=)))
            (small-sep (line 70)))
        (dotimes (i (max (length errorlogs)
                         (length errordumps)))
          (format fd "~a~4%~a~%  HIBAÜZENET:~%~a~4%~a~4%~a~%  BACKTRACE:~%~a~4%~a~8%"
                  big-sep small-sep small-sep
                  (nth i errorlogs)
                  small-sep small-sep
                  (nth i errordumps)))
      (get-output-stream-string fd)))))


;; ----------------------------------------------------------------------
;; Execution

(defmethod enable-errorsink ((obj wax-app))
  (setf (errorsink-enabled-p obj) t))

(defmethod disable-errorsink ((obj wax-app))
  (setf (errorsink-enabled-p obj) nil))

(defmethod set-execute-fn ((obj wax-app) fn)
  (setf (execute-fn obj) fn))

(defmethod wax-execute ((obj wax-app) &key (errorsink-on t) (args '()))
  "Start the function stored in the EXECUTE-FN slot of OBJ with the errorsink active or not."
  (if errorsink-on
    (enable-errorsink obj)
    (disable-errorsink obj))
  (with-wax-errorsink (obj)
    (funcall (execute-fn obj) obj args)))




;; ----------------------------------------------------------------------
;; Sandbox










#.(disable-ccom-syntax)

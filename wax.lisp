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
  (:documentation "Data sources used during script execution."))

(defmethod load-src ((obj data-source) &optional (header-height 1))
  (with-slots ((filename filename)) obj
    (when (and filename
               (string/= filename "")
               (probe-file filename))
      (setf (data obj)
            (with-workbook (:open filename :read-only t :wsvars (wsheet) :close t)
              (read-xarray (used-range wsheet) :from-row (1+ header-height)))
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
;; Script state class

(defparameter *state-file* "state" "Textfile to store script state between runs.")

(defclass wax-script ()
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
  (:documentation "Wax script environment."))


;; ----------------------------------------------------------------------
;; Progress window methods

(defmethod disp ((obj wax-script) control-string &rest args)
  (apply (disp-fn obj) control-string args))

;(defmethod set-pstep-limit ((obj wax-script) limit)
;  (setf (pstep-limit obj) limit))

(defmethod pstep ((obj wax-script) &key (abs nil) (step 1))
  (funcall (pstep-fn obj) :abs abs :step step))

(defmethod pabort ((obj wax-script))
  "Stop the progress loop by user intent."
  (funcall (pabort-fn obj)))

(defmethod pkill ((obj wax-script))
  "Stop the progress loop from a WITH-WAX-ERRORSINK clause."
  (funcall (pkill-fn obj)))


;; ----------------------------------------------------------------------
;; State permanency & handling

(defmethod save-state ((obj wax-script) &key (package-name "WAX") (keys '() keys-provided-p))
  (let* ((filename (appfile *state-file* package-name))
         (state    (state obj))
         (plist    (if keys-provided-p
                     (keep-pairs state keys)
                     state)))
    (when (probe-file filename)
      (hide-file filename nil))
    (save-forms filename plist)))

(defmethod load-state ((obj wax-script) &key (package-name "WAX") (keys '() keys-provided-p))
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

(defmethod init-state ((obj wax-script) &rest plist)
  (setf (state obj) plist))

(defmethod get-state ((obj wax-script) key)
  (getf (state obj) key))

(defun (setf get-state) (value obj key)
  (setf (getf (state obj) key) value))


;; ----------------------------------------------------------------------
;; Handling data sources

(defmethod add-data-source ((obj wax-script) key filename)
  (setf (data-sources obj)
        (override-pairs
         (data-sources obj)
         (list key (make-instance 'data-source :filename filename)))))

(defmethod remove-data-source ((obj wax-script) key)
  (setf (data-sources obj)
        (remove-pairs (data-sources obj) (list key))))

(defmethod load-data-source ((obj wax-script) key &optional (header-height 1))
  (load-src (getf (data-sources obj) key) header-height))

(defmethod purge-data-source ((obj wax-script) key)
  (purge (getf (data-sources obj) key)))

(defmethod select-row-from ((obj wax-script) key selector-fn)
  (let ((data-source (getf (data-sources obj) key)))
    (when (loaded-p data-source)
      (select-row data-source selector-fn))))

(defmethod source-filename ((obj wax-script) key)
  (filename (getf (data-sources obj) key)))

(defmethod source-data ((obj wax-script) key)
  (data (getf (data-sources obj) key)))


;; ----------------------------------------------------------------------
;; Error messages


;; General
(defmethod queue-message ((obj wax-script) getter setter string)
  (funcall setter (cons string (funcall getter obj)) obj))

(defmethod messages-waiting-p ((obj wax-script) getter)
  (not (zerop (length (funcall getter obj)))))

(defmethod disp-messages ((obj wax-script) getter)
  (dolist (message (reverse (funcall getter obj)))
    (disp obj message))
  (disp obj "~6%"))

(defmethod purge-messages ((obj wax-script) setter)
  (funcall setter '() obj))
;  (setf (funcall accessor obj) '()))


;; Errorlogs
(defmethod queue-errorlog ((obj wax-script) string)
  (queue-message obj #'errorlogs #'(setf errorlogs) string))

(defmethod errorlogs-waiting-p ((obj wax-script))
  (messages-waiting-p obj #'errorlogs))

(defmethod disp-errorlogs ((obj wax-script))
  (disp-messages obj #'errorlogs))

(defmethod purge-errorlogs ((obj wax-script))
  (purge-messages obj #'(setf errorlogs)))


;; Errordumps
(defmethod queue-errordump ((obj wax-script) string)
  (queue-message obj #'errordumps #'(setf errordumps) string))

(defmethod errordumps-waiting-p ((obj wax-script))
  (messages-waiting-p obj #'errordumps))

(defmethod disp-errordumps ((obj wax-script))
  (disp-messages obj #'errordumps))

(defmethod purge-errordumps ((obj wax-script))
  (purge-messages obj #'(setf errordumps)))


;; Dump both streams into a single string.
(defmethod fulldump ((obj wax-script))
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

(defmethod set-execute-fn ((obj wax-script) fn)
  (setf (efecute-fn obj) fn))

;; EZT NEM HASZNÁLJUK, KELL ???
#|(defmethod init-wax-script ((obj wax-script) &key
                            (state '())
                            (errorsink-on nil)
                            (prop-accessors-on t)
                            (execute-fn nil))
  (init-state obj state)
  (setf (errorsink-on) errorsink-on)
  (setf (property-accessors-on) prop-accessors-on)
  (set-execute-fn obj execute-fn))|#

(defmethod wax-execute ((obj wax-script) &key (errorsink-on nil) (property-accessors-on t))
  (with-wax-errorsink obj
    (with-property-accessors
      (setf (errorsink-on) errorsink-on
            (property-accessors-on) property-accessors-on)
      (funcall (execute-fn obj) obj))))


;; ----------------------------------------------------------------------
;; Sandbox

#|(defun jj ()
  (let ((script (make-instance
                 'wax-script
                 :state '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6)
                 :execute-fn #'(lambda (obj)
                                 (with-progress-new ("Hihi" obj (/ (length (state obj)) 2))
                                   (loop for (k v) on (state obj) by #'cddr doing
                                         (disp obj "~a  ~a~%" k v)
                                         (disp obj "~a~%" (get-state obj k))
                                         (setf (get-state obj k) "Grr")
                                         (disp obj "~a~%" (get-state obj k))
                                         (pstep obj)
                                         (pabort obj)
                                         (sleep 1)))
                                 (save-state obj)))))
    (wax-execute script)))|#


#|(defun kk ()
  (let* ((file "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\TK vezetõk.xlsx")
         (obj  (make-instance 'data-source :filename file)))
    (print (loaded-p obj))
    (load-src obj)
    (print (loaded-p obj))
    (let ((row (select-row obj #'(lambda (row)
                                   (astring= (xcref row "TK") "Észak-Pesti Tankerületi Központ")))))
      (print (xcref row "Helységnév")))))|#


#|(defun ll ()
  (let ((obj (make-instance
              'wax-script
              :state '(:a 1 :b 2 :c 3 :d 4 :e 5 :f 6)
              :execute-fn
              #'(lambda (obj)
                  (with-progress-new ("Haladás!" obj 2)
                    (disp obj "KIR tábla betöltése~%")
                    (load-data-source obj :kir)
                    (let ((row (select-row-from
                                obj :kir #'(lambda (row)
                                             (and (= (parse-number (xcref row "OM azonosító"))
                                                     40003)
                                                  (= (parse-number (xcref row "A feladatellátási hely sorszáma"))
                                                     5))))))
                      (disp obj "Eredmény: ~a~%" (xcref row "A feladatellátási hely pontos címe")))
                    (disp obj "TK vezetõ tábla betöltése~%")
                    (load-data-source obj :tks)
                    (let ((row (select-row-from
                                obj :tks
                                #'(lambda (row)
                                    (string= (xcref row "TK") "Szigetszentmiklósi Tankerületi Központ")))))
                      (disp obj "Eredmény: ~a~%" (xcref row "Gazdasági vez.")))
                    (sleep 5))))))
    (add-data-source obj :kir "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\kir_mukodo_feladatellatasi_helyek_2025_01_06.xlsx")
    (add-data-source obj :tks "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\TK vezetõk.xlsx")
    (wax-execute obj)))|#

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
  (if (loaded-p obj)
    (xaselect (data obj) selector-fn)
    (error "Data source ~a not loaded." (filename obj))))


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
   (dump-fn
    :accessor dump-fn)
   (pstep-limit
    :accessor pstep-limit)
   (pstep-fn
    :accessor pstep-fn)
   (pabort-fn
    :accessor pabort-fn)
   (pkill-fn
    :accessor pkill-fn)
   (data-sources
    :accessor data-sources
    :initform '()))
  (:documentation "Wax script environment."))


;; ----------------------------------------------------------------------
;; Progress window methods

(defmethod dump ((obj wax-script) control-string &rest args)
  (apply (dump-fn obj) control-string args))

;(defmethod set-pstep-limit ((obj wax-script) limit)
;  (setf (pstep-limit obj) limit))

(defmethod pstep ((obj wax-script) &key (abs nil) (step 1))
  (funcall (pstep-fn obj) :abs abs :step step))

(defmethod pabort ((obj wax-script))
  (funcall (pabort-fn obj)))

(defmethod pkill ((obj wax-script))
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
                                         (dump obj "~a  ~a~%" k v)
                                         (dump obj "~a~%" (get-state obj k))
                                         (setf (get-state obj k) "Grr")
                                         (dump obj "~a~%" (get-state obj k))
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
                    (dump obj "KIR tábla betöltése~%")
                    (load-data-source obj :kir)
                    (let ((row (select-row-from
                                obj :kir #'(lambda (row)
                                             (and (= (parse-number (xcref row "OM azonosító"))
                                                     40003)
                                                  (= (parse-number (xcref row "A feladatellátási hely sorszáma"))
                                                     5))))))
                      (dump obj "Eredmény: ~a~%" (xcref row "A feladatellátási hely pontos címe")))
                    (dump obj "TK vezetõ tábla betöltése~%")
                    (load-data-source obj :tks)
                    (let ((row (select-row-from
                                obj :tks
                                #'(lambda (row)
                                    (string= (xcref row "TK") "Szigetszentmiklósi Tankerületi Központ")))))
                      (dump obj "Eredmény: ~a~%" (xcref row "Gazdasági vez.")))
                    (sleep 5))))))
    (add-data-source obj :kir "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\kir_mukodo_feladatellatasi_helyek_2025_01_06.xlsx")
    (add-data-source obj :tks "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\TK vezetõk.xlsx")
    (wax-execute obj)))|#

;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;; ----------------------------------------------------------------------
;; Things that should go into CCOM


;; ----------------------------------------------------------------------
;; Excel stuff


(defun xcol-values (wsheet column &key (start 2))
  (let* ((coln   (resolve-column-designator column wsheet))
         (result (xrange wsheet coln start coln (last-row wsheet))))
    (if (and (arrayp result) (not (stringp result)))
      (column->row result)
      (make-array 1 :initial-element result))))


(defun xcol-uniques (wsheet column &key (start 2) (test :auto))
  (let ((test-fn  (cond ((eq test :auto) #'equalp)
                        ((typep test 'function) test)
                        (t (error "TEST must be a function or :AUTO.")))))
    (remove-duplicates (xcol-values wsheet column :start start) :test test-fn)))


(defmacro xdouniq ((e wsheet column &key (start 2) (test :auto) (select '())) &body body)
  (let ((filtered (gensym)))
    `(cclet* ((,filtered (if ,select
                           (xselect> ,wsheet ,select)
                           ,wsheet)))
       (loop for ,e across (xcol-uniques ,filtered ,column :start ,start :test ,test) doing
             ,@body))))


(defun xcol-header (xarray title)
  (let ((vec (make-array (array-dimension xarray 1) :displaced-to xarray)))
    (position title vec :test #'string=)))


;; Column designation: same as with XCELL.
;; Row designation: index only. :(
(defun xaref (xarray x y)
  (let* ((xx (typecase x
               (integer x)
               (keyword (1- (letters-column x)))
               (string  (xcol-header xarray x)))))
    (aref xarray y xx)))


;; ----------------------------------------------------------------------
;; csd utility stuff


;; CL universal time -> ISO-8601.
(defun timestamp (ut &key (timeshift 0))
  (let* ((timestamp  (local-time:timestamp+
                      (local-time:universal-to-timestamp ut)
                      timeshift :hour))
         (timestring (local-time:format-timestring nil timestamp
                                                   :format local-time:+iso-8601-format+)))
    (subseq (cl-ppcre::regex-replace-all ":" timestring "-")
            0 19)))


(defun words-capitalized (string)
  (format nil "~:(~a~)" string))


(defparameter *word-separators* '(#\Space))

(defun split-into-words (string &optional (separators *word-separators*))
  (labels ((non-space (string &optional (start 0))
             (or (position-if #'(lambda (char)
                                  (not (member char separators)))
                              string :start start)
                 (length string)))
           (space (string &optional (start 0))
             (or (position-if #'(lambda (char)
                                  (member char separators))
                              string :start start)
                 (length string)))
           (worker (string result)
             (if (string= string "")
                 result
               (let* ((word-start (non-space string))
                      (word-end   (space string word-start))
                      (rest-start (non-space string word-end))
                      (word       (subseq string word-start word-end))
                      (rest       (subseq string rest-start)))
                 (worker rest (cons word result))))))
    (nreverse (worker string '()))))


(defun conc-with-single-spaces (list)
  (let ((laced '()))
    (dolist (word list)
      (push word laced)
      (push " " laced))
    (apply #'concatenate
           (cons 'string
                 (butlast (nreverse laced))))))


(defun remove-double-spaces (string)
  (conc-with-single-spaces
   (split-into-words string)))


(defun trim-edge-spaces (string)
  (labels ((non-space (char)
             (char/= char #\Space)))
    (let ((start (or (position-if #'non-space string) 0))
          (end   (or (position-if #'non-space string :from-end t)
                     (length string))))
      (subseq string start (1+ end)))))


(defun clean-name (string)
  (words-capitalized
   (trim-edge-spaces
    (remove-double-spaces string))))

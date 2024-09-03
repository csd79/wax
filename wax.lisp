;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;; ----------------------------------------------------------------------
;; Things that should go into CCOM


(defun begining-of-doc (document)
  #p(first #p(characters document)))


(defun end-of-doc (document)
  #p(last #p(characters document)))


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


(defun excel-date (n)
  (let* ((a    (+ n 2483588))
         (b    (truncate (/ (* a 4) 146097)))
         (c    (- a (truncate (/ (+ (* 146097 b) 3) 4))))
         (d    (truncate (/ (* 4000 (+ c 1)) 1461001)))
         (e    (+ (- c (+ (truncate (/ (* 1461 d) 4)))) 31))
         (f    (truncate (/ (* 80 e) 2447)))
         (day  (- e (truncate (/ (* 2447 f) 80))))
         (g    (truncate (/ f 11)))
         (mon  (- (+ f 2) (* 12 g)))
         (year (+ (* 100 (- b 49)) d g)))
    (list year mon day)))


(defun excel-date-string (n &key (words nil))
  (destructuring-bind (year mon day)
      (excel-date (truncate n))
    (if words
      (let ((months '("január" "február" "március" "április" "május" "június" "július"
                      "augusztus" "szeptember" "október" "november" "december")))
        (format nil "~4d. ~a ~d." year (nth (1- mon) months) day))
    (format nil "~4d.~2,'0d.~2,'0d." year mon day))))


;; ----------------------------------------------------------------------
;; Excel stuff


;; List values from given column.
(defun list-column-values (wsheet column-designator &key (first 2) (last :last) (select '()))
  (let ((column (resolve-column-designator column-designator wsheet)))
    (unless column
      (error "Column ~a cannot be found in worksheet ~a." column-designator wsheet))
    (if select
      ;; SELECT is present, iterationg over given rows.
      (loop for r in select collecting
            (xcell wsheet column r))
      ;; Otherwise, extracting rows between START and LAST-ROW.
      (let ((last-row (cond ((integerp last) last)
                            ((eq last :last) (last-row wsheet))
                            (t (error "Keyword argument :LAST must be an integer (row number) or :LAST.")))))
        (when (< last-row first)
          (error "LAST-ROW (~a) is smaller then FIRST (~a)." last-row first))
        (coerce
         (column->row
          (xrange wsheet column first column last-row))
         'list)))))


;; List unique values from given column.
(defun list-unique-column-values (wsheet column-designator &key (first 2) (last :last) (test :auto) (select '()))
  (let* ((all-values (list-column-values wsheet column-designator :first first :last last :select select))
         (test-fn    (cond ((eq test :auto) #'equalp)
                           ((typep test 'function) test)
                           (t (error "TEST must be a function or :AUTO.")))))
    (declare (ignore wsheet))
    (remove-duplicates all-values :test test-fn)))


;; List row numbers with given values in designated columns. Example:
;;   (select-rows wsheet
;;                '("SZK" "B9")
;;                '("Születési hely" "Budapest") ...)
;; ->
;;   (19 24 32 38 66)
(defun select-rows (wsheet &rest subscripts)
  (flet ((single (column-designator value)
           (let ((values  (list-column-values wsheet column-designator)))
             (loop for i from 0 below (length values)
                   when (equalp (nth i values) value)
                   collect (+ i 2)))))
    (sort (reduce #'nintersection
                  (loop for (col val) in subscripts collecting
                        (single col val)))
          #'<)))


;; A more comfortable version of LIST-UNIQUE-COLUMN-VALUES. SELECT must be a flat list of subscripts.
(defun xuniq (wsheet col-designator &key (first 2) (last :last) (test :auto) (select '()))
  ;; Split subscripts into (column value) pairs.
  (let ((subscripts (loop for a in select by #'cddr
                          for b in (rest select) by #'cddr collecting (list a b))))
    
    (if subscripts
      (let ((selected (apply #'select-rows wsheet subscripts)))
        ;; If subscripts yielded a selection, list values from it, otherwise return an empty list.
        (when selected
          (list-unique-column-values wsheet col-designator :first first :last last :test test
                                     :select selected)))
      ;; If subscripts are empty, list values while ignoring them.
      (list-unique-column-values wsheet col-designator :first first :last last :test test :select '()))))


;; Iterate over unique values from column COLUMND in the WSHEET worksheet,
;; according to selection.
(defmacro xdouniq ((i wsheet columnd &key (first 2) (last :last) (test :auto) (select '())) &body body)
  (let ((list (gensym)))
    `(let ((,list (xuniq ,wsheet ,columnd :first ,first :last ,last :test ,test :select ,(cons 'list select))))
       (dolist (,i ,list)
         ,@body))))



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


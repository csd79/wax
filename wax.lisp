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


(defun add-article (word)
  (let* ((clean (trim-edge-spaces (remove-double-spaces word)))
         (vowels '(#\a #\á #\e #\é #\i #\í #\o #\ó #\ö #\õ #\u #\ú #\ü #\û #\A #\Á #\E #\É #\I #\Í #\O #\Ó #\Ö #\Õ #\U #\Ú #\Ü #\Û))
         (article (if (position (elt clean 0) vowels :test #'char=)
                    "az" "a")))
    (concatenate 'string article " " clean)))


(defun group->word (number)
  (let* ((ones     '("egy" "kettõ" "három" "négy" "öt" "hat" "hét" "nyolc" "kilenc"))
         (tens     '("tíz" "húsz" "harminc" "negyven" "ötven" "hatvan" "hetven" "nyolcvan" "kilencven"))
         (tens+    '("tizen" "huszon" "harminc" "negyven" "ötven" "hatvan" "hetven" "nyolcvan" "kilencven"))
         (hundreds '("egyszáz" "kettõszáz" "háromszáz" "négyszáz" "ötszáz" "hatszáz" "hétszáz" "nyolcszáz" "kilencszáz"))
         (result   '())
         (a        (truncate number 100))
         (b        (- (truncate number 10) (* a 10)))
         (c        (- number (* a 100) (* b 10))))
    (unless (zerop c)
      (push (nth (1- c) ones) result))
    (unless (zerop b)
      (push (nth (1- b) (if (zerop c) tens tens+)) result))
    (unless (zerop a)
      (push (nth (1- a) hundreds) result))
    (apply #'concatenate 'string result)))
    

(defun sub->words (number)
  (unless number
    (error "~a is not a number." number))
  (when (> number 999999999)
    (error "The value ~a is larger than 999 999 999."))
  (if (zerop number)
    "nulla"
    (let* ((result '())
           (a      (truncate number 1000000))
           (b      (- (truncate number 1000) (* a 1000)))
           (c      (- number (* a 1000000) (* b 1000))))
      (unless (zerop c)
        (push (group->word c) result))
      (unless (zerop b)
        (push (concatenate 'string (group->word b) "ezer-") result))
      (unless (zerop a)
        (push (concatenate 'string (group->word a) "millió-") result))
      (let* ((final  (apply #'concatenate 'string result))
             (length (length final)))
        (if (char= (elt final (1- (length final))) #\-)
          (subseq final 0 (- length 1))
          final)))))
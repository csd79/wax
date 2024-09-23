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


(defun empty-cell-p (value)
  (or (and (stringp value)
           (string= value ""))
      (and (symbolp value)
           (member value '(empty :empty)))
      (null value)))


;; ----------------------------------------------------------------------
;; Word stuff


(defun select-bookmark (document bookmark &key (if-not-found :skip))
  (multiple-value-bind (value error)
      (ignore-errors
        #m(select #m(item #p(bookmarks document) bookmark))
        #p(selection #p(activewindow document)))
    (case if-not-found
      (:skip  value)
      (:error (if (not error)
                value
                (error error)))
      (t      (error ":IF-NO-FOUND should be either :SKIP or :ERROR.")))))


(defun overwrite-bookmark (document bookmark string &key (if-not-found :skip))
  ;; TYPETEXT will not overwrite selection if the new string is ""...
  (cclet* ((notnull (if (string= string "")
                      " "
                      string))
           (select  (select-bookmark document bookmark :if-not-found if-not-found)))
    (when select
      #m(typetext select notnull)
      (when (string= string "")
        #m(typebackspace select)))))



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





(defparameter *word-separators* '(#\Space))

#|(defun split-into-words (string &optional (separators *word-separators*))
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
    (nreverse (worker string '()))))|#


#|(defun conc-with-single-spaces (list)
  (let ((laced '()))
    (dolist (word list)
      (push word laced)
      (push " " laced))
    (apply #'concatenate
           (cons 'string
                 (butlast (nreverse laced))))))|#


#|(defun remove-double-spaces (string)
;  (conc-with-single-spaces
  (str:unwords
;   (split-into-words string)))
   (str:words string)))|#


#|(defun trim-edge-spaces (string)
  (labels ((non-space (char)
             (char/= char #\Space)))
    (let ((start (or (position-if #'non-space string) 0))
          (end   (or (position-if #'non-space string :from-end t)
                     (length string))))
      (subseq string start (1+ end)))))|#


(defun clean-name (string)
  (astring-capitalize
;  (words-capitalized
;   (trim-edge-spaces
   (str:trim
;    (remove-double-spaces string))))
    (str:unwords (str:words string)))))


(defun add-article (word)
;  (let* ((clean (trim-edge-spaces (remove-double-spaces word)))
  (let* ((clean (str:trim (str:unwords (str:words word))))
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


(defun currency (number)
  (format nil "~,,' ,3:d" (round number)))




#|
;; ----------------------------------------------------------------------
;; Character ordering & case support


(defparameter *hu-alpha*
  #((#\a . #\A) (#\á . #\Á) (#\b . #\B) (#\c . #\C) (#\d . #\D) (#\e . #\E)
    (#\é . #\É) (#\f . #\F) (#\g . #\G) (#\h . #\H) (#\i . #\I) (#\í . #\Í)
    (#\j . #\J) (#\k . #\K) (#\l . #\L) (#\m . #\M) (#\n . #\N) (#\o . #\O)
    (#\ó . #\Ó) (#\ö . #\Ö) (#\õ . #\Õ) (#\p . #\P) (#\q . #\Q) (#\r . #\R)
    (#\s . #\S) (#\t . #\T) (#\u . #\U) (#\ú . #\Ú) (#\ü . #\Ü) (#\û . #\Û)
    (#\v . #\V) (#\w . #\W) (#\x . #\X) (#\y . #\Y) (#\z . #\Z)))


(defparameter *achar-table* *hu-alpha*)


(defun achar-table-width ()
  (length *achar-table*))


(defun alpha-achar-p (char)
  (position-if #'(lambda (pair)
                   (or (char= char (car pair))
                       (char= char (cdr pair))))
               *achar-table*))
  

(defun achar-code (char)
  (let* ((found-pos (alpha-achar-p char))
         (offset    (if (and found-pos
                             (char= char (first (elt *achar-table* found-pos))))
                      (achar-table-width)
                      0)))
    (when found-pos
      (+ offset found-pos))))


(defun code-achar (code)
  (let ((width (achar-table-width)))
    (cond ((< code width)
           (cdr (elt *achar-table* code)))
          ((< code (+ width width))
           (car (elt *achar-table* (- code width))))
          (t nil))))


(defun achar= (char1 char2)
  (= (achar-code char1)
     (achar-code char2)))


(defun achar< (char1 char2)
  (< (achar-code char1)
     (achar-code char2)))


(defun achar-equal (char1 char2)
  (or (achar= char1 char2)
      (= (abs (- (achar-code char1)
                 (achar-code char2)))
         (achar-table-width))))


(defun achar-lessp (char1 char2)
  (let* ((width      (achar-table-width))
         (code1      (achar-code char1))
         (code2      (achar-code char2))
         (code1-norm (if (>= code1 width)
                       (- code1 width)
                       code1))
         (code2-norm (if (>= code2 width)
                       (- code2 width)
                       code2)))
    (< code1-norm code2-norm)))
         

(defun achar-upper-case-p (char)
  (let ((width (achar-table-width))
        (code  (achar-code char)))
    (if (not code)
      (upper-case-p char)
      (and (<= 0 code)
           (< code width)))))


(defun achar-lower-case-p (char)
  (let ((width (achar-table-width))
        (code  (achar-code char)))
    (if (not code)
      (lower-case-p char)
      (and (<= width code)
           (< code (* width 2))))))


(defun achar-upcase (char)
  (cond ((achar-lower-case-p char)
         (code-achar (- (achar-code char)
                        (achar-table-width))))
        ((achar-upper-case-p char)
         char)
        (t (char-upcase char))))


(defun achar-downcase (char)
  (cond ((achar-upper-case-p char)
         (code-achar (+ (achar-code char)
                        (achar-table-width))))
        ((achar-lower-case-p char)
         char)
        (t (char-downcase char))))


(defun achar/= (char1 char2)
  (not (achar= char1 char2)))


(defun achar> (char1 char2)
  (and (not (achar= char1 char2))
       (not (achar< char1 char2))))


(defun achar<= (char1 char2)
  (not (achar> char1 char2)))


(defun achar>= (char1 char2)
  (not (achar< char1 char2)))


(defun achar-not-equal (char1 char2)
  (not (achar-equal char1 char2)))


(defun achar-greaterp (char1 char2)
  (and (not (achar-equal char1 char2))
       (not (achar-lessp char1 char2))))


(defun achar-not-greaterp (char1 char2)
  (not (achar-greaterp char1 char2)))


(defun achar-not-lessp (char1 char2)
  (not (achar-lessp char1 char2)))


(defun astring-comparison (string1 string2 equality &optional (unequality nil))
  (let* ((len (min (length string1) (length string2)))
         (pos (loop for i from 0 below len doing
                    (unless (funcall equality (char string1 i) (char string2 i))
                      (loop-finish))
                      finally (return i))))
;    (print pos)
    (if (not unequality)
      (= pos len)
      (when (< pos len)
        (funcall unequality (char string1 pos) (char string2 pos))))))


(defun astring= (string1 string2)
  (astring-comparison string1 string2 #'achar=))


(defun astring-equal (string1 string2)
  (astring-comparison string1 string2 #'achar-equal))


(defun astring/= (string1 string2)
  (not (astring= string1 string2)))


(defun astring-not-equal (string1 string2)
  (not (astring-equal string1 string2)))


(defun astring< (string1 string2)
  (astring-comparison string1 string2 #'achar= #'achar<))


(defun astring> (string1 string2)
  (and (not (astring< string1 string2))
       (not (astring= string1 string2))))


(defun astring<= (string1 string2)
  (not (astring> string1 string2)))


(defun astring>= (string1 string2)
  (not (astring< string1 string2)))


(defun astring-lessp (string1 string2)
  (astring-comparison string1 string2 #'achar-equal #'achar-lessp))


(defun astring-greaterp (string1 string2)
  (and (not (astring-equal string1 string2))
       (not (astring-lessp string1 string2))))


(defun astring-not-greaterp (string1 string2)
  (not (astring-greaterp string1 string2)))


(defun astring-not-lessp (string1 string2)
  (not (astring-lessp string1 string2)))


(defun astring-upcase (string)
  (let* ((len (length string))
         (new (make-string len)))
    (loop for i from 0 below len doing
          (setf (char new i)
                (achar-upcase (char string i))))
    new))


(defun astring-downcase (string)
  (let* ((len (length string))
         (new (make-string len)))
    (loop for i from 0 below len doing
          (setf (char new i)
                (achar-downcase (char string i))))
    new))


(defun astring-capitalize (string)
  (let* ((len (length string))
         (new (astring-downcase string))
         (upc t))
    (loop for i from 0 below len
          for current = (char new i) doing
          (cond ((achar-lower-case-p current)
                 (when upc
                   (setf (char new i) (achar-upcase current))
                   (setf upc nil)))
                ((not (alpha-achar-p current))
                 (setf upc t))
                (t nil)))
    new))
|#            




#|
; ¡¡¡¡¡¡ FOOOOOOOS! ¡¡¡¡¡¡


(defun words-capitalized (string)
  (format nil "~:(~a~)" string))

(defun words-capitalized (string)
  (let ((words (split-into-words string)))
    (conc-with-single-spaces
     (mapcar #'(lambda (word)
                 (concatenate 'string
                              (list (char-upcase (elt word 0)))
                              (string-downcase (subseq word 1))))
             words))))


(defparameter *hu-case* '(("á" "Á") ("é" "É") ("í" "Í") ("ó" "Ó") ("ö" "Ö") ("õ" "Õ") ("ú" "Ú") ("ü" "Ü") ("û" "Û")))

(defun hupcase (string)
  (let ((stage1   (string-upcase string)))
    (labels ((repl (string list)
               (if list
                 (destructuring-bind (from to) (first list)
                   (repl (cl-ppcre::regex-replace-all from string to)
                         (rest list)))
                 string)))
      (repl stage1 *hu-case*))))


(defun hdncase (string)
  (let ((stage1   (string-downcase string)))
    (labels ((repl (string list)
               (if list
                 (destructuring-bind (to from) (first list)
                   (repl (cl-ppcre::regex-replace-all from string to)
                         (rest list)))
                 string)))
      (repl stage1 *hu-case*))))
|#

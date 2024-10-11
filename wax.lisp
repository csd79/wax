;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;; ----------------------------------------------------------------------
;; Things that should go into CCOM


(defun empty-cell-p (value)
  (or (and (stringp value)
           (string= value ""))
      (and (symbolp value)
           (member value '(empty :empty)))
      (null value)))


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


(defun clean-city (string)
  (let ((words (str:words string)))
    (str:unwords
     (cons (astring-capitalize (first words))
           (rest words)))))


(defun clean-name (string)
  (astring-capitalize
   (str:trim
    (str:unwords (str:words string)))))


(defun add-article (word)
  (let* ((clean (str:trim (str:unwords (str:words word))))
         (vowels '(#\a #\á #\e #\é #\i #\í #\o #\ó #\ö #\õ #\u #\ú #\ü #\û #\A #\Á #\E #\É #\I #\Í #\O #\Ó #\Ö #\Õ #\U #\Ú #\Ü #\Û))
         (article (if (position (elt clean 0) vowels :test #'char=)
                    "az" "a")))
    (concatenate 'string article " " clean)))


(defun group->word (orig-number)
  (let* ((number   (round orig-number))
         (ones     '("egy" "kettõ" "három" "négy" "öt" "hat" "hét" "nyolc" "kilenc"))
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
    

(defun sub->words (orig-number)
  (unless orig-number
    (error "~a is not a number." orig-number))
  (let ((number (round orig-number)))
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
            final))))))


(defun currency (number)
  (format nil "~,,' ,3:d" (round number)))

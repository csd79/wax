;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*-


(in-package :wax)


;; ----------------------------------------------------------------------
;; History


(defparameter *independent-exe* nil "Modify APPDIR's behaviour.")
(defparameter *dev-dir* "c:\\Users\\cselovszkid\\common-lisp\\wax\\" "System dir on dev machine.")

(defun appdir ()
  "Namestring of the directory containing wax."
  (if *independent-exe*
      (namestring (lw:current-pathname))
      *dev-dir*))

(defun appfile (file)
  "Namestring of FILE inside wax's directory."
  (merge-pathnames file (appdir)))

(defun load-forms (file)
  "Load Lisp forms from FILE into a list."
  (with-open-file (in file
                      :direction :input
                      :if-does-not-exist nil)
    (when in
      (read in nil))))
#|      (loop for f = (read in nil)
            until (null f)
            collecting f))))|#

(defun save-forms (file &rest forms)
  "Save Lisp forms from FORMS into FILE."
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (dolist (form forms)
      (prin1 form out))))


;; ----------------------------------------------------------------------
;; Plists


(defun keep-pairs-worker (plist keys-to-keep &optional (acc '()))
  "Build a copy of PLIST, but remove every pair whose key is not listed in KEYS-TO-KEEP."
  (multiple-value-bind (key val rest)
      (get-properties plist keys-to-keep)
    (if key
        (keep-pairs-worker (cddr rest) keys-to-keep (append (list key val) acc))
      acc)))

(defun keep-pairs (plist &optional (keys-to-keep '()))
  "Return a copy of PLIST, but remove every pair whose key is not listed in KEYS-TO-KEEP. If KEYS-TO-KEEP is empty, return PLIST unchanged."
  (if keys-to-keep
      (keep-pairs-worker plist keys-to-keep)
    plist))

(defun remove-pairs (plist &optional (keys-to-remove '()))
  "Return a copy of PLIST, but remove every pair whose key is a member of KEYS-TO-REMOVE."
  (if keys-to-remove
      (loop for (key value) on plist by #'cddr
            unless (member key keys-to-remove)
            collect (list key value) into result
            finally (return (apply #'nconc result)))
    plist))

(defun modify-keys (plist &optional (keys-to-modify '()))
  "Return a copy of PLIST with the keys modified according to the contents of KEYS-TO-MODIFY (:old-key :new-key ...)"
  (if keys-to-modify
      (loop for (key value) on plist by #'cddr
            collect (list (or (getf keys-to-modify key)
                              key)
                          value) into result
            finally (return (apply #'nconc result)))
    plist))
                                     
(defun override-pairs (plist &optional (new-pairs '()))
  "Append NEW-PAIRS to PLIST, overriding any pairs with the same key."
  (if new-pairs
      (let* ((keys-to-override (loop for (key) on new-pairs by #'cddr
                                     collecting key))
             (pruned-plist (loop for (key value) on plist by #'cddr
                                 unless (member key keys-to-override)
                                 collect (list key value) into result
                                 finally (return (apply #'nconc result)))))
        (append pruned-plist new-pairs))
    plist))

(defun drop-nils (plist)
  "Remove any pairs whose value is nil."
  (loop for (key value) on plist by #'cddr
        unless (null value)
        collect (list key value) into list1
        finally (return (apply #'nconc list1))))

(defun modify-plist (plist &key (keep '()) (remove '()) (modify '()) (override '()) (drop-nils nil))
  "Return modified copy of PLIST using the above fns."
  (let* ((result1 (keep-pairs plist keep))
         (result2 (remove-pairs result1 remove))
         (result3 (modify-keys result2 modify))
         (result4 (override-pairs result3 override)))
    (if drop-nils
        (drop-nils result4)
      result4)))


;; ----------------------------------------------------------------------
;; Date, time


(defun timestamp (ut &key (timeshift 1))
  "Convert unversal time to ISO-8601."
  (let* ((timestamp  (local-time:timestamp+
                      (local-time:universal-to-timestamp ut)
                      timeshift :hour))
         (timestring (local-time:format-timestring nil timestamp
                                                   :format local-time:+iso-8601-format+)))
    (subseq (cl-ppcre::regex-replace-all ":" timestring "-")
            0 19)))

(defun identify-month (string)
  "Identify textual month of the year in hungarian or english."
  (when (stringp string)
    (let* ((mon '(("jan" "january" "januar" "január")
                  ("feb" "february" "februar" "február")
                  ("mar" "már" "march" "marcius" "március")
                  ("apr" "ápr" "april" "aprilis" "április")
                  ("maj" "máj" "may" "majus" "május")
                  ("jun" "jún" "june" "junius" "június")
                  ("jul" "júl" "july" "julius" "július")
                  ("aug" "august" "augusztus")
                  ("sep" "szep" "szept" "september" "szeptember")
                  ("okt" "oct" "october" "oktober" "október")
                  ("nov" "november")
                  ("dec" "december")))
           (pos (position-if
                 #'(lambda (sublist)
                     (member string sublist :test #'astring-equal))
                 mon)))
      (when pos (1+ pos)))))

(defun parse-hudate (string)
  "Convert hungarian short textual date into a list (year month day)."
  (destructuring-bind (year month day)
      (multiple-value-bind (full vector)
          (cl-ppcre:scan-to-strings
           "^.*(\\d{2,4})[^a-zA-z\\d:]+(\\d{1,2}|[\\p{L}\\p{M}]+)[^a-zA-z\\d:]+(\\d{1,2}).*$" string)
        (declare (ignore full))
        (coerce vector 'list))
    (let* ((year-raw (parse-integer year))
           (year-ok  (if (< year-raw 100)
                       (+ 2000 year-raw)
                       year))
           (month-ok (or (parse-integer month :junk-allowed t)
                         (identify-month month))))
      (list year-ok month-ok (parse-integer day)))))

(defun hudate->unitime (hudatelist)
  "Convert the list (year month day) to Lisp universal time."
  (apply #'encode-universal-time
         0 0 0 (reverse hudatelist)))


;; ----------------------------------------------------------------------
;; Currency


(defun group->word (orig-number)
  "Convert a group of three digits into hungarian textual number."
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
  "Convert an integer between 1 and 999 999 999 to hungarian text."
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
  "Convert integer into textual number with groups of three digits."
  (format nil "~,,' ,3:d" (round number)))


;; ----------------------------------------------------------------------
;; Clean text


(defun clean-city (string)
  "String but with first word capitalized, with no leading, trailing or double spaces."
  (let ((words (str:words string)))
    (str:unwords
     (cons (astring-capitalize (first words))
           (rest words)))))

(defun clean-name (string)
  "STRING capitalized, with no leading, trailing or double spaces."
  (astring-capitalize
   (str:trim
    (str:unwords (str:words string)))))

(defun add-article (word)
  "Ensure proper hungarian article before WORD."
  (let* ((clean (str:trim (str:unwords (str:words word))))
         (vowels '(#\a #\á #\e #\é #\i #\í #\o #\ó #\ö #\õ #\u #\ú #\ü #\û #\A #\Á #\E #\É #\I #\Í #\O #\Ó #\Ö #\Õ #\U #\Ú #\Ü #\Û))
         (article (if (position (elt clean 0) vowels :test #'char=)
                    "az" "a")))
    (concatenate 'string article " " clean)))

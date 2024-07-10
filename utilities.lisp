;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*-

(in-package :wax)


#|
(defun remove-delimited-substrings (string open close)
  (let ((start (search open string :test #'string=)))
    (if start
      (let* ((length (length string))
             (end  (or (search close string :test #'string= :start2 start)
                       length string))
             (pre  (if (zerop start)
                     ""
                     (subseq string 0 start)))
             (post (if (= end length)
                     ""
                     (subseq string (+ end (length close))))))
        (remove-delimited-substrings
         (concatenate 'string pre post)
         open close))
      string)))
|#


(defun replace-1substring (string old new &optional (start 0))
  (let ((start (search old string :start2 start)))
    (if start
        (let* ((end    (+ start (length old)))
               (before (subseq string 0 start))
               (after  (subseq string end)))
          (concatenate 'string before new after))
      string)))

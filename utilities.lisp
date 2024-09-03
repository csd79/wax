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


#|
(defun replace-1substring (string old new &optional (start 0))
  (let ((start (search old string :start2 start)))
    (if start
        (let* ((end    (+ start (length old)))
               (before (subseq string 0 start))
               (after  (subseq string end)))
          (concatenate 'string before new after))
      string)))
|#


#|
(defun compile-and-load (source-file fasl-directory)
  (let* ((filename  (pathname-name source-file))
         (fasl-file (make-pathname :name filename :type "fasl" :defaults fasl-directory)))
;    (compile-file source-file :output-file fasl-file :load t)))
    (compile-file source-file :output-file fasl-file)
    (load fasl-file)))
|#


#|
(defun hotload-files (source-files source-dir fasl-directory)
  (dolist (source-file source-files)
    (compile-and-load (make-pathname :name source-file :type "lisp" :defaults source-dir)
                      fasl-directory)))
|#


#|
(defun hotload-ccom ()
  (hotload-files '("package"
                   "utilities"
                   "syntax"
                   "excel"
                   "word"
                   "ppoint"
                   "sandbox")
                 "c:\\Users\\cselovszkid\\common-lisp\\ccom\\"
                 "c:\\Users\\cselovszkid\\common-lisp\\ccom\\fasl\\"))
|#


#|
  Error: Cannot read character Backspace as part of a token because it has constituent trait 'invalid'.
  1 (continue) Try loading c:\Users\cselovszkid\common-lisp\ccom\fasl\package.fasl again.
  2 Give up loading c:\Users\cselovszkid\common-lisp\ccom\fasl\package.fasl.
  3 Try loading another file instead of c:\Users\cselovszkid\common-lisp\ccom\fasl\package.fasl.
  4 (abort) Return to top loop level 0.
|#

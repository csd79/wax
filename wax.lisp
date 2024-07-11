;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;; ----------------------------------------------------------------------
;; Global vars


(defparameter *eol*   (format nil "~C" #\return)) ; End of row in a Word doc

(defparameter *batch*   nil)
(defparameter *current* nil)

(defparameter *infile*  nil) ; File for process input
(defparameter *outfile* nil) ; File for process output



;;; ----------------------------------------------------------------------
;;; Utilities


;;; Generate automatic outfile name if *OUTFILE* is NIL.
(defun auto-outfile (infile)
  (let* ((in-str (namestring infile))
         (in-name (pathname-name in-str))
         (out-name (concatenate 'string in-name "_")))
    (make-pathname :name out-name :defaults infile)))


;;; ----------------------------------------------------------------------
;;; Processing Word document


;;; List all simple and multiline comments in STRING.
(defun list-comments (string)
  (labels ((list-cmmnts (string open close &optional (start 0) (comments '()))
             (let ((start (search open string :test #'string= :start2 start)))
               (if start
                 (let* ((len (length string))
                        (end (search close string :test #'string= :start2 start))
                        (end2 (+ end (length close)))
                        (end3 (if (and (< end2 len)
                                       (char= (elt string end2)
                                              (elt *eol* 0)))
                                (1+ end2)
                                end2)))
                   (list-cmmnts
                    string open close end
                    (cons (subseq string start (min end3 len))
                          comments)))
                 (nreverse comments)))))
        (append (list-cmmnts string "#;" *eol*)
                (list-cmmnts string "#{" "#}"))))


;;; Searches SEQUENCE for multiple prosperous SUBSEQS,
;;; return the position and identity of the earliest one.
(defun search-any (subseqs sequence &key (test #'equalp))
  ;; Trying to find first occurance of each subseq
  (let ((positions  (mapcar #'(lambda (subseq)
                                (search subseq sequence :test test))
                            subseqs))
        (min-pos    nil)
        (min-subseq nil))
    ;; Determining first occurrance of any subseq
    (mapc #'(lambda (subseq position)
              (when position
                (if min-pos
                  ;; There were previous finds
                  (when (< position min-pos)
                    (setf min-pos position
                          min-subseq subseq))
                  ;; First find
                  (setf min-pos position
                        min-subseq subseq))))
          subseqs positions)
    (values min-pos min-subseq)))


;;; Extract inserts from STRING, replace them with placeholders.
;;; Return the processed string and the extracted expressions as LET*-clauses.
(defun doc-processor (string &optional (exp-pairs '())
                             (repl-subs '()) (remove-subs '()))
  ;; Look for inserts
  (multiple-value-bind (start subseq)
      (search-any '("#&" "#@") string)
    (if start
      ;; If found, add expression to EXP-PAIRS
      (multiple-value-bind (expression next)
          (read-from-string string nil nil
                            :start (+ start (length subseq))
                            :preserve-whitespace t)
        (let* ((symbol      (gensym))
               ;; Store expression
               (exp-pairs   (append exp-pairs
                                    (list
                                     (list symbol expression))))
               (exp-string  (subseq string start next))
               ;; Store replacable expression if subseq=#@
               (repl-subs   (if (string= subseq "#@")
                              (append repl-subs
                                      (list
                                       (list exp-string symbol)))
                              repl-subs))
               ;; Store removable expression if subseq=#&
               (remove-subs (if (string= subseq "#&")
                              (append remove-subs
                                      (list exp-string))
                              remove-subs))
               ;; Remove insert from string
               (string      (replace-1substring string exp-string "")))
          (doc-processor string exp-pairs repl-subs remove-subs)))
      ;; No more inserts
      (values exp-pairs repl-subs remove-subs))))


;;; Construct the evaluator expression for the document.
(defun construct-fn (expressions body)
  (let ((head '())
        (neck '()))
    (loop for (sym exp) in expressions doing
          (if (and (listp exp)
                   (eq (first exp) 'local))
            (push (second exp) head)
            (push (list sym exp) neck)))
    (let ((neck (nreverse neck)))
      (print (list 'cclet* (apply #'append (nreverse head))
                   (append (list 'let* neck)
                           body))))))



;;; ----------------------------------------------------------------------
;;; Processing modes


(defun batch-mode (file sheet title)
  (if *batch*
    ;; Iteratives already extracted
    (progn
      (setf *current* (first *batch*)
            *batch*   (rest *batch*))
      (format t "already running~%next: ~a~%batch: ~a~%" *current* *batch*))
    ;; Extract iteratives
    (cclet* ((wbook (get-document file))
             (wsheets #p(worksheets wbook))
             (wsheet  #p(item wsheets sheet))
             (column  (title-column wsheet title)))
      (with-used-edges (wsheet left top right bottom)
        (let* ((col    #p(value2 (range wsheet column 2 column bottom)))
               (list   (loop for i from 0 below (array-dimension col 0)
                             collecting (aref col i 0)))
               (unique (remove-duplicates list :test #'equalp)))
          (setf *current* (first unique)
                *batch*   (rest unique))
          (format t "starting batch~%first: ~a~%batch: ~a~%" *current* *batch*))))))



(defun process-single-document (infile)
  (setf *infile* infile)
  (cclet* ((word      (com:create-object :progid "Word.Application"))
           (documents #p(documents word))
           (document  #m(open documents infile))
           (text      #p(text #p(content document)))
           (comments  (list-comments text)))
    (unwind-protect
        (progn
          ;; Remove comments
          (dolist (comment comments)
            (word-replace-text document comment ""))
          (multiple-value-bind (expr-clauses placeholders removables)
              (doc-processor text)
            ;; Remove #&s
            (dolist (removable removables)
              (word-replace-text document removable ""))
            ;; Construct embedded script
            (let* ((body  (mapcar 
                           #'(lambda (placeholder)
                               (append '(word-replace-text document) placeholder))
                           placeholders))
                   (whole (construct-fn expr-clauses body)))
              ;; Evaluate script
              (progv '(document) (list document)
                (eval whole)))))
      ;; Save results
      (let ((outfile (namestring (or *outfile*
                                     (auto-outfile infile)))))
        (format t "procsf: current: ~a~%outfile: ~a~%" *current* outfile)
        #m(saveas2 document outfile)
        #m(close document 0))))
  ;; Reset & continue batch (if any)
  (when *batch*
    (process-single-document infile)))


;;; ----------------------------------------------------------------------
;;; Sandbox


(defparameter *s* "c:\\Users\\cselovszkid\\common-lisp\\wax\\teszt.docx")


(defun test1 ()
  (unwind-protect
      (process-single-document *s*)
    (setf *batch* nil)))
  
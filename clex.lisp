;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 


(in-package #:clex)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Seeking workbooks and worksheets by name


;; Return a list of full pathnames of all open Excel files
(defun open-workbook-fullnames ()
  (excel (xl)
    (comlet* ((workbooks (com-property xl "Workbooks"))
              (count     (com-property workbooks "Count")))
      (loop for i from 1 upto count collecting
            (comlet* ((workbook (com-property workbooks "Item" i)))
              (com-property workbook "FullName"))))))


;; Turn full pathname into simple filename
(defun fullname->name (fullname)
  (format nil "~a.~a"
          (pathname-name fullname)
          (pathname-type fullname)))


;; Return a list of the names of every worksheet in an open Excel file
(defun worksheet-names (workbook-fullname)
  (excel (xl)
    (comlet* ((workbooks (com-property xl "Workbooks"))
              (workbook  (com-property workbooks "Item" (fullname->name workbook-fullname))))
      (when workbook
        (comlet* ((worksheets (com-property workbook "Worksheets"))
                  (count      (com-property worksheets "Count")))
        (loop for i from 1 upto count collecting
              (comlet* ((worksheet (com-property worksheets "Item" i)))
                (com-property worksheet "Name"))))))))


;; Return a list of every worksheet in every open Excel file
(defun all-available-worksheets ()
  (comlet* ((workbook-fullnames (open-workbook-fullnames))
            (results            '()))
    (dolist (fullname workbook-fullnames)
      (push fullname results)
      (push (worksheet-names fullname) results))
    (nreverse results)))
#| ((wb1 ws1)
    (wb1 ws2)
    (wb2 ws1)
    (wb2 ws2)
    ...|#


;(defun select-worksheet ()
;  (let ((available (all-available-worksheets)))


;; Return interface pointer for worksheet described by given filename and sheetname
(defun grab-worksheet (workbook-fullname worksheet-name)
  (excel (xl)
    (comlet* ((workbooks  (com-property xl "Workbooks"))
              (workbook   (com-property workbooks "Item" (fullname->name workbook-fullname)))
              (worksheets (com-property workbook "Worksheets")))
      (com-property worksheets "Item" worksheet-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Range of interes class


(defclass roi ()
  ((fullname  :initarg :fullname  :accessor fullname)
   (sheetname :initarg :sheetname :accessor sheetname)
   (x1        :initarg :x1        :accessor x1        :initform nil)
   (y1        :initarg :y1        :accessor y1        :initform nil)
   (x2        :initarg :x2        :accessor x2        :initform nil)
   (y2        :initarg :y2        :accessor y2        :initform nil)))









#|

(get-active-object :progid "SAPGUI" :riid 'i-dispatch :errorp nil)

Error: COM::GET-ACTIVE-OBJECT-FROM-CLSID : Cannot find CLSID for PROGID "SAPGUI".
  1 (abort) Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options"SAPGUI"


(get-active-object :progid "SAPGUI.Application" :riid 'i-dispatch)

Error: COM Error (GET-ACTIVE-OBJECT) : A mûvelet nem hajtható végre

  1 (abort) Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template 
|#
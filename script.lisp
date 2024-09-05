;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;;; ----------------------------------------------------------------------
;;; Ki/bemeneti fájlok


(defparameter *xls-query* "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\wax-EXPORT.XLSX")
(defparameter *xls-tks* "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\TK vezetõk.xlsx")
(defparameter *doc-template-dir* "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\")

(defparameter *out-dir* "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Eredmény\\")

(defparameter *templates-ps*
  '(
;    ("B1" "")
    ("B2" "Pedagógus_kinevezési okmány.docx")
;    ("B8" "Ped szakkép_noks_Púétv_kinevezési okmány.docx")
    ("B9" "Nem ped szakkép_noks_Púétv_kinevezési okmány.docx")
    ))


;;; ----------------------------------------------------------------------
;;; Sablonok kezelése


(defun doctemplate (ps)
  (concatenate 'string *doc-template-dir*
               (second (find ps *templates-ps* :key #'first :test #'string=))))


(defun newfile (tk ps)
  (format nil "~a~a ~a ~a ~a"
          *out-dir* tk ps
          (timestamp (get-universal-time))
          (second (find ps *templates-ps* :key #'first :test #'string=))))


(defun tempfile ()
  (format nil "~a~a_~a~a"
          *out-dir* "temp" (timestamp (get-universal-time)) ".docx"))


;;; ----------------------------------------------------------------------
;;; Törzs


(defparameter *page-break-needed* nil)


#|(defmacro extrfn (colds &body body)
  `#'(lambda (wsheet sztsz)
       (let ,(mapcar #'(lambda (pair)
                         `(,(first pair)
                           (xcell wsheet ,(second pair) (list "SZTSZ" sztsz))))
                     colds)
         ,@body)))|#


(defmacro getfn (colds &body body)
  `#'(lambda (xarray)
       (let ,(mapcar #'(lambda (pair)
                         `(,(first pair)
                           (xaref xarray ,(second pair) 1)))
                     colds)
         ,@body)))


(defparameter *t2*
  `(("$01$" ,(getfn ((a "Név"))
               (clean-name a)))
    
    ("$02$" ,(getfn ((a "Születési vezetéknév") (b "Születési utónév") (c "2.születési utónév"))
               (clean-name (conc-with-single-spaces (list a b c)))))
    
    ("$03$" ,(getfn ((a "Születési hely") (b "Születési dátum"))
               (concatenate 'string (clean-name a) ", " (excel-date-string b :words t))))

    ("$04$" ,(getfn ((a "Anya") (b "Anyja keresztneve") (c "Anyja 2.keresztneve"))
               (clean-name (conc-with-single-spaces (list a b c)))))
))


#|(defparameter *t2*
  `(("$01$" ,(extrfn ((a "Név"))
               (clean-name a)))
    
    ("$02$" ,(extrfn ((a "Születési vezetéknév") (b "Születési utónév") (c "2.születési utónév"))
               (clean-name (conc-with-single-spaces (list a b c)))))
    
    ("$03$" ,(extrfn ((a "Születési hely") (b "Születési dátum"))
               (concatenate 'string (clean-name a) ", " (excel-date-string b :words t))))

    ("$04$" ,(extrfn ((a "Anya") (b "Anyja keresztneve") (c "Anyja 2.keresztneve"))
               (clean-name (conc-with-single-spaces (list a b c)))))
))|#


(defun fill-template (current xarray)
  (dolist (pair *t2*)
    (destructuring-bind (old new-fn)
        pair
      (word-replace-text current old (funcall new-fn xarray)))))


#|(defun fill-template (tmpdoc xarray)
  (format t "~a    ~a    ~a~%"
          (xaref xarray "Vállalat hosszú megnevezése" 1)
          (xaref xarray "SZK" 1)
          (xaref xarray "SZTSZ" 1)))|#


(defun add-template (doc xarray)
  (setf x xarray)
  (let ((ps    (xaref xarray "SZK" 1))
        (tmp   (tempfile)))
    ;; Új temp file dok.sablon alapján
    (ccom::with-document (current :open-file (doctemplate ps) :close t :save t)
      #m(saveas2 current tmp)
      ;; Temp sablon feltöltése a lekérdezésbõl
      (fill-template current xarray))
    ;; Temp sablon tartalmának beillesztése az eredménybe
    (if *page-break-needed*
      #m(insertbreak (end-of-doc doc))
      (setf *page-break-needed* t))
    #m(insertfile (end-of-doc doc) tmp)
    ;; Temp törlése
    (delete-file tmp))
  ;; Eredmény állapotának mentése
  #m(save doc)
  ;; Progress bar
  (format t "~a~%" (xaref xarray "SZTSZ" 1)))


(defun start ()
  (with-workbook (wbook :open-file *xls-query* :read-only t :wsvars (ws-query) :close t)
    (let ((tk-head "Vállalat hosszú megnevezése"))
      ;; Iteráció TK-kon.
      (xdouniq (tk ws-query tk-head)
        ;; Iteráció személyi körökön.
        (xdouniq (ps ws-query "SZK" :select `((,tk-head ,tk)))
          ;; Új dokumentum létrehozása, mentés másként
          (ccom::with-document (newdoc :close t :save t)
            #m(saveas2 newdoc (newfile tk ps))
            ;; Iteráció SZTSZ-eken.
            ;; Oldaltörés inicializálása.
            (setf *page-break-needed* nil)
            (xdouniq (sztsz ws-query "SZTSZ" :select `((,tk-head ,tk) ("SZK" ,ps)))
              ;; SZTSZ adatainak beírása a dokumentumba.
              (add-template newdoc
                            #p(value2 (used-range (xselect> ws-query
                                                            `(("SZTSZ" ,sztsz)))))))))))))


;;; ----------------------------------------------------------------------
;;; Sandbox


#|(defun test09-fill (worksheet document sztsz)
  (cclet* ((range #m(range document 0 0)))
    #m(insertafter range sztsz)
    #m(insertafter range "  ")
    (format t "~a~%" sztsz)))

(defun test09 ()
  (with-workbook (wbook :open-file *xls-query* :wsvars (ws-query) :close t)
    (let ((tk-head "Vállalat hosszú megnevezése"))
      ;; Iteráció TK-kon.
      (xdouniq (tk ws-query tk-head)
        ;; Iteráció személyi körökön.
        (dolist (ps (mapcar #'first *templates-ps*))
          ;; Új dokumentum létrehozása, mentés másként
          (ccom::with-document (newdoc :open-file (doctemplate ps) :close t :save t)
            #m(saveas2 newdoc (newfile tk ps))
            ;; Iteráció SZTSZ-eken.
            (xdouniq (sztsz ws-query "SZTSZ" :select (tk-head tk "SZK" ps))
              ;; SZTSZ adatainak beírása a dokumentumba.
              (test09-fill ws-query newdoc sztsz))))))))



(defun test10 ()
  (with-workbook (wbook :open-file *xls-query* :wsvars (ws-query) :close t)
    (cclet* ((filtered (xselect> ws-query '(("Vállalat hosszú megnevezése" "Sziget*")
                                            ("SZK" "B2")
                                            ))))
      (loop for r from 2 upto (last-row filtered) doing
            (format t "~a~%" (xcell filtered "SZTSZ" r))))
    ))


(defun test11 ()
  (with-workbook (wbook :open-file *xls-query* :wsvars (ws-query) :close t)
    (xdouniq (e ws-query "Vállalat hosszú megnevezése")
      (print e))))


(defun test12 ()
  (with-workbook (wbook :open-file *xls-query* :read-only t :wsvars (ws-query) :close t)
    (let ((tk-head "Vállalat hosszú megnevezése"))
       (xdouniq (tk ws-query tk-head)
         (xdouniq (ps ws-query "SZK" :select `((,tk-head ,tk)))
             (xdouniq (sztsz ws-query "SZTSZ" :select `((,tk-head ,tk) ("SZK" ,ps)))
               (format t "~a   ~a   ~a~%" tk ps sztsz)))))))|#



#|
"Bérelem"/"Rövid szöveg", "Összeg", "Kezdete" és "Vége" oszlopok beazonosítása
------------------------------------------------------------------------------
Egyértelmû
----------
Bérelem + utána Rövid szöveg: IT0008
Bérelem + utána Bérelem: IT0014
Összeg értéke néha 0 és ugyanazokbana sorokban az egyik Kezdete/Vége oszlop is üres: IT0014

Valószínû
---------
Összeg: IT0008 átlaga kb 4x a 0014 átlagának
Vége: szórás IT0014-en több mint 20x IT0008-nak
Kezdete: szórás valamivel alacsonyabb IT0008-on
|#

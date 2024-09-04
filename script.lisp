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


(defmacro extrfn (colds &body body)
  `#'(lambda (wsheet sztsz)
       (let ,(mapcar #'(lambda (pair)
                         `(,(first pair)
                           (xcell wsheet ,(second pair) (list "SZTSZ" sztsz))))
                     colds)
         ,@body)))


#|(defparameter *t1* `(("$01$" ,#'(lambda (wsheet sztsz)
                                  (clean-name (xcell wsheet "Név" `("SZTSZ" ,sztsz)))))

                     ("$02$" ,#'(lambda (wsheet sztsz)
                                  (clean-name
                                   (conc-with-single-spaces
                                    (list (xcell wsheet "Születési vezetéknév" `("SZTSZ" ,sztsz))
                                          (xcell wsheet "Születési utónév"     `("SZTSZ" ,sztsz))
                                          (xcell wsheet "2.születési utónév"   `("SZTSZ" ,sztsz)))))))

                     ("$03$" ,#'(lambda (wsheet sztsz)
                                  (concatenate 'string
                                               (clean-name
                                                (xcell wsheet "Születési hely"  `("SZTSZ" ,sztsz)))
                                               ", "
                                               (excel-date-string
                                                (xcell wsheet "Születési dátum" `("SZTSZ" ,sztsz))
                                                :words t))))

                     ("$04$" ,#'(lambda (wsheet sztsz)
                                  (clean-name
                                   (conc-with-single-spaces
                                    (list (xcell wsheet "Anya"                `("SZTSZ" ,sztsz))
                                          (xcell wsheet "Anyja keresztneve"   `("SZTSZ" ,sztsz))
                                          (xcell wsheet "Anyja 2.keresztneve" `("SZTSZ" ,sztsz)))))))
))|#


(defparameter *t2*
  `(("$01$" ,(extrfn ((a "Név"))
               (clean-name a)))
    
    ("$02$" ,(extrfn ((a "Születési vezetéknév") (b "Születési utónév") (c "2.születési utónév"))
               (clean-name (conc-with-single-spaces (list a b c)))))
    
    ("$03$" ,(extrfn ((a "Születési hely") (b "Születési dátum"))
               (concatenate 'string (clean-name a) ", " (excel-date-string b :words t))))

    ("$04$" ,(extrfn ((a "Anya") (b "Anyja keresztneve") (c "Anyja 2.keresztneve"))
               (clean-name (conc-with-single-spaces (list a b c)))))
))





(defun fill-template (worksheet current sztsz)
  (dolist (pair *t2*)
    (destructuring-bind (old new-fn)
        pair
      (word-replace-text current old (funcall new-fn worksheet sztsz)))))



(defun add-template (worksheet document sztsz)
  (let ((ps  (xcell worksheet "SZK" `("SZTSZ" ,sztsz)))
        (tmp (tempfile)))
    ;; Új temp file dok.sablon alapján
    (ccom::with-document (current :open-file (doctemplate ps) :close t :save t)
      #m(saveas2 current tmp)
      ;; Temp sablon feltöltése a lekérdezésbõl
      (fill-template worksheet current sztsz))
    ;; Temp sablon tartalmának beillesztése az eredménybe
    (if *page-break-needed*
      #m(insertbreak (end-of-doc document))
      (setf *page-break-needed* t))
    #m(insertfile (end-of-doc document) tmp)
    ;; Temp törlése
    (delete-file tmp))
  ;; Eredmény állapotának mentése
  #m(save document)
  ;; Progress bar
  (format t "~a~%" sztsz))  


(defun start ()
  ;, Oldaltörés inicializálása.
  (setf *page-break-needed* nil)
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
            (xdouniq (sztsz ws-query "SZTSZ" :select `((,tk-head ,tk) ("SZK" ,ps)))
              ;; SZTSZ adatainak beírása a dokumentumba.
              (add-template ws-query newdoc sztsz))))))))


;;; ----------------------------------------------------------------------
;;; Sandbox


(defun test09-fill (worksheet document sztsz)
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
               (format t "~a   ~a   ~a~%" tk ps sztsz)))))))



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

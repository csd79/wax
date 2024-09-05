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


(defmacro const-fn (colds &body body)
  `#'(lambda (xarray)
       (let ,(mapcar #'(lambda (pair)
                         `(,(first pair)
                           (xaref xarray ,(second pair) 1)))
                     colds)
         ,@body)))


(defun get-fee (xarray row cols codes)
  (let ((width (array-dimension xarray 0)))
    (apply #'append
           (mapcar #'(lambda (col code)
                       (list code (when (> width col)
                                    (xaref xarray col row))))
                   cols codes))))

(defun get-fees (xarray)
  (let ((result '())
        (width  (array-dimension xarray 0))
        (codes  '(:code :name :sum :start :end)))
    (loop for r from 1 below width doing
          (push (get-fee xarray r '(15 16 17 35 29) codes) result)
          (push (get-fee xarray r '(19 20 21 34 30) codes) result))
#|          (push (list :code  (xaref xarray 15 r)
                      :name  (xaref xarray 16 r)
                      :sum   (xaref xarray 17 r)
                      :start (when (> width 35)
                               (xaref xarray 35 r))
                      :end   (xaref xarray 29 r))
                result)
          (push (list :code  (xaref xarray 19 r)
                      :name  (xaref xarray 20 r)
                      :sum   (xaref xarray 21 r)
                      :start (when (> width 34)
                               (xaref xarray 34 r))
                      :end   (xaref xarray 30 r))
                result))|#
    (remove-duplicates result :test #'equalp)))


(defun find-fee (code fees)
  (find-if #'(lambda (record)
               (string= code (getf record :code))) fees))


(defmacro fees-fn ((fees) &body body)
  `#'(lambda (xarray)
       (let ((,fees (get-fees xarray)))
         ,@body)))


(defun read-tk-data (tk)
  (with-workbook (wbook :open-file *xls-tks* :read-only t :wsvars (help) :close t)
    (let ((row (locate-row help "TK" tk #'string=)))
      (cons tk (mapcar #'(lambda (title)
                           (xcell help title row))
                       '("Helységnév" "TK ig" "Gazdasági vez." "Törzsszám" "Székhely"))))))

(defparameter *tk-data* nil)

(defun get-tk-data (tk)
  (when (or (null *tk-data*)
            (string/= (first *tk-data*) tk))
    (setf *tk-data* (read-tk-data tk)))
  *tk-data*)






;; EZT ÚGY KÉNE MÓDOSÍTANI, HOGY CSAK AKKOR KERESSEN A FEJLÉCBEN/LÁBLÉCBEN, HA EXPLICITE JELEZVE VAN!
(defparameter *t2*
  `(("$01$" ,(const-fn ((a "Név"))
               (clean-name a)))
    
    ("$02$" ,(const-fn ((a "Születési vezetéknév") (b "Születési utónév") (c "2.születési utónév"))
               (clean-name (conc-with-single-spaces (list a b c)))))
    
    ("$03$" ,(const-fn ((a "Születési hely") (b "Születési dátum"))
               (concatenate 'string (clean-name a) ", " (excel-date-string b :words t))))

    ("$04$" ,(const-fn ((a "Anya") (b "Anyja keresztneve") (c "Anyja 2.keresztneve"))
               (clean-name (conc-with-single-spaces (list a b c)))))

    ("$05$" ,(const-fn ((a "Belépés dátuma"))
               (excel-date-string a :words t)))

    ("$06$" ,(const-fn ((a "Munkakör"))
               (remove-double-spaces
                (trim-edge-spaces a))))

    ("$07$" ,(const-fn ((a "szervezeti egys hosszú megnev."))
               (remove-double-spaces 
                (trim-edge-spaces a))))

    ("$08$" ,(const-fn ((a "Heti óra"))
               (round a)))

    ("$09$" ,(const-fn ((a "FEOR-sz.s."))
               (remove-double-spaces 
                (trim-edge-spaces a))))

    ("$10$" ,(const-fn ((a "Bérrendsz. csop név"))
               (remove-double-spaces 
                (trim-edge-spaces a))))

    ("$11$" ,(const-fn ((a "Belépés dátuma"))
               (excel-date-string a :words t)))

    ("$14$" ,(const-fn ((a "Vállalat hosszú megnevezése"))
               (add-article
                (first (split-into-words a)))))

    ("$15$" ,(const-fn ((a "Kinevezés/szerzõdés jellege"))
               (if (string= a "Határozatlan id.kine")
                 ""
                 "és 40. § (1)-(3) bekezdése ")))

    ("$16$" ,(const-fn ((a "Kinevezés/szerzõdés jellege") (b "Szerz.vége"))
               (if (string= a "Határozatlan id.kine")
                 "határozatlan idejû"
                 (format nil "tartósan távollévõ helyettesítése céljából határozott ideig, várhatóan ~a napjáig tartó" (excel-date-string b :words t)))))

    ("$12$" ,(const-fn ((a "Vállalat hosszú megnevezése"))
               (second (get-tk-data a))))

    ("$13$" ,(const-fn ((a "Vállalat hosszú megnevezése"))
               (second (get-tk-data a))))

    ("$17$" ,(const-fn ((a "Vállalat hosszú megnevezése"))
               (third (get-tk-data a))))

    ("$18$" ,(const-fn ((a "Vállalat hosszú megnevezése"))
               (fourth (get-tk-data a))))
    
    ("$19$" ,(const-fn ((a "Név"))
               (clean-name a)))

    ("$20$" ,(const-fn ((a "Vállalat hosszú megnevezése"))
               (sixth (get-tk-data a))))
    
    ("$21$" ,(const-fn ((a "Vállalat hosszú megnevezése"))
               (round (fifth (get-tk-data a)))))

    ("$22$" ,(const-fn ((a "Vállalat hosszú megnevezése"))
               (string-upcase (first (split-into-words (first (get-tk-data a)))))))

    ("$23$" ,(const-fn ((a "Vállalat hosszú megnevezése"))
               (third (get-tk-data a))))

    ("$24$" ,(fees-fn (fees)
               (format nil "~,,' ,3:d" (getf (find-fee "1100" fees) :sum))))

    ("$25$" ,(fees-fn (fees)
               (let ((sum (getf (find-fee "1100" fees) :sum)))
                 (when sum
                   (sub->words sum)))))

#|    ("$26$" ,(const-fn ((szk "SZK") (bd "Belépés dátuma") (pv "Próbaidõ  vége"))
               (let* ((pv-str (if (numberp pv)
                                (excel-date-string pv :words t)
                                "..."))
                      (period (concatenate
                               'string
                               (excel-date-string bd :words t)
                               "napjától "
                               pv-str)))
                 (if (string= szk "B1")
                   ;; B1
                   (if (string= pv "")
                     ""
                     (concatenate
                      'string
                      "A munka törvénykönyvérõl szóló 2012. évi I. törvény (a továbbiakban: Mt.) 45. § (5) bekezdése alapján a felek "
                      period
                      "napjáig terjedõ próbaidõt kötnek ki, amely idõtartam alatt a munkaviszonyt az Mt. 79. § (1) bekezdésének a) pontja alapján bármelyik fél azonnali hatályú felmondással – indokolás nélkül – megszüntetheti.~%"))
                   ;; Egyéb személyi körök
                   (if (string= pv "")
                     "A ….-jogszabály-….alapján próbaidõ nem köthetõ ki.~%"
                     (concatenate
                      'string
                      "A Púétv. 41. § (1) bekezdése alapján "
                      period
                      " napjáig tartó próbaidõt kötök ki, amely idõtartam alatt a köznevelési foglalkoztatotti jogviszonyt a Púétv. 41. § (4) bekezdése és 46. § (2) bekezdésének a) pontja alapján bármelyik fél indokolás nélkül azonnali hatállyal megszüntetheti.~%"))))))|#
                   
))

    

#|
HIÁNYZÓ ÉRTÉKEK
gyakornoki idõ          B2   B8
illetmény jogsz.hiv.    B2   B8   B9
illetmény elemei        B2   B8   B9
|#


(defun fill-template (current xarray) ; 12%
;  (print (get-fees xarray));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (dolist (pair *t2*)
    (destructuring-bind (old new-fn)
        pair
      (word-replace-text current old (funcall new-fn xarray)))))


(defconstant +wd-section-break-next-page+ 2)
(defconstant +wd-header-footer-first-page+ 2)


(defun add-template (doc xarray) ; 46%
  (let ((ps  (xaref xarray "SZK" 1))
;        (tmp (tempfile))
        )
    ;; Új temp file dok.sablon alapján
    (with-document (current :open-file (doctemplate ps) :close t)
;      #m(saveas current tmp)
      ;; Adatok beillesztése táblázatból
      (fill-template current xarray)
      (if *page-break-needed*
        #m(insertbreak (end-of-doc doc) +wd-section-break-next-page+)
        (setf *page-break-needed* t))
      ;; Jelen SZTSZ dok.törzs másolása
      #m(select current)
      #m(copy #p(selection #p(parent current)))
      #m(paste (end-of-doc doc))
      ;; 1. oldali fejléc/lábléc másolása
      (cclet* ((sect-src #p(first #p(sections current)))
               (sect-trg #p(last  #p(sections doc)))
               (head-src #p(range #m(item #p(headers sect-src) +wd-header-footer-first-page+)))
               (head-trg #p(range #m(item #p(headers sect-trg) +wd-header-footer-first-page+)))
               (foot-src #p(range #m(item #p(footers sect-src) +wd-header-footer-first-page+)))
               (foot-trg #p(range #m(item #p(footers sect-trg) +wd-header-footer-first-page+))))
        #m(copy  head-src)
        #m(paste head-trg)
        #m(copy  foot-src)
        #m(paste foot-trg)
        (setf #p(differentfirstpageheaderfooter #p(pagesetup sect-trg)) t)))
;    (delete-file tmp)
    )
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
          (with-document (newdoc :close t :save t)
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


(defparameter *src* "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\sandbox\\Adatokkal_Pedagógus_kinevezési okmány.docx")
(defparameter *rst* "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\sandbox\\Érdi TK 2024.09.09. #01 B2.docx")


(defun test01 ()
  (with-document (doc1 :open-file *src* :close t)
    (cclet* ((sections #p(sections doc1))
             (section  #m(item sections 1))
             (header1  #m(item #p(headers section) +wd-header-footer-first-page+))
             (footer1  #m(item #p(footers section) +wd-header-footer-first-page+)))
      (print #p(count sections))
      (print header1)
      (print footer1))))


(defun test02 ()
  (with-document (doc1 :open-file *src* :close t)
    (cclet* ((sections #p(sections doc1))
             (section  #m(item sections 1))
             (header1  #m(item #p(headers section) +wd-header-footer-first-page+))
             (footer1  #m(item #p(footers section) +wd-header-footer-first-page+)))
      (print #p(count sections))
      (print #p(text #p(range header1)))
      (print #p(text #p(range footer1))))))




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

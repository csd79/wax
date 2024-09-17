;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;;; ----------------------------------------------------------------------
;;; Globális változók


(defparameter *xls-tks-filename* "TK vezetõk.xlsx")

(defparameter *xls-query*        "")
(defparameter *xls-query2*       "")
(defparameter *doc-template-dir* "")
(defparameter *out-dir*          "")
(defparameter *xls-tks*          "")

(defparameter *doctype*          "")


;;; ----------------------------------------------------------------------
;;; Generálható dokumentumtípusok


(defun szk-fn (szk)
  #'(lambda (xarray)
      (string= (xaref xarray "SZK" 1) szk)))

(defun b1-noks-fn ()
  #'(lambda (xarray)
      (and (string= (xaref xarray "SZK" 1) "B1")
           (member (xaref xarray "Munkakör" 1) '("4336" "4211" "4214") :test #'string=))))

(defun b1-kiseg-fn ()
  #'(lambda (xarray)
      (and (string= (xaref xarray "SZK" 1) "B1")
           (not (member (xaref xarray "Munkakör" 1) '("4336" "4211" "4214") :test #'string=)))))

(defparameter *doctypes*
  `(
    (:name "Kinevezések"
     :dir  "Kinevezések"
     :szk
     ((,(szk-fn "B2") "B2" "Pedagógus_kinevezési okmány.docx")
      (,(szk-fn "B8") "B8" "Ped szakkép_noks_Púétv_kinevezési okmány.docx")
      (,(szk-fn "B9") "B9" "Nem ped szakkép_noks_Púétv_kinevezési okmány.docx")
      (,(b1-noks-fn)  "B1" "Munkaszerzõdés_noks munkakör_munkavállaló.docx") 
      (,(b1-kiseg-fn) "B1" "Munkaszerzõdés_gazd., ügyv., mûsz.,kiseg.munkakör_munkavállaló.docx")))

    (:name "Egyoldalú kinevezésmódosítások"
     :dir  "Egyoldalú kinevezésmódosítások"
     :szk
     ((,(szk-fn "B2") "B2" "Kinevezésmódosítás_egyoldalú_pedagógus.docx")
      (,(szk-fn "B8") "B8" "Kinevezésmódosítás_egyoldalú_ped. szakkép. noks.docx")
      (,(szk-fn "B9") "B9" "Kinevezésmódosítás_egyoldalú_nem ped. szakkép. noks.docx")))

    (:name "Kétoldalú kinevezésmódosítás"
     :dir  "Kétoldalú kinevezésmódosítás"
     :szk
     ((,(szk-fn "B2") "B2" "Kinevezésmódosítás_kétoldalú_pedagógus.docx")
      (,(szk-fn "B8") "B8" "Kinevezésmódosítás_kétoldalú_ped. szakkép. noks.docx")
      (,(szk-fn "B9") "B9" "Kinevezésmódosítás_kétoldalú_nem ped. szakkép. noks.docx")
      (,(b1-noks-fn)  "B1" "Munkaszerzõdés-módosítás_noks munkakör_munkavállaló.docx")
      (,(b1-kiseg-fn) "B1" "Munkaszerzõdés-módosítás_gazd., ügyv., mûsz.,kiseg.munkakör_munkavállaló.docx")))))


#|(defparameter *templates-ps*
  '(
;    ("B1" "")
    ("B2" "Pedagógus_kinevezési okmány.docx")
;    ("B8" "Ped szakkép_noks_Púétv_kinevezési okmány.docx")
;    ("B9" "Nem ped szakkép_noks_Púétv_kinevezési okmány.docx")
    ))|#


;;; ----------------------------------------------------------------------
;;; Sablonok kezelése


(defun select-doctype (xarray)
  (when (string/= *doctype* "")
    (let* ((type (find *doctype* *doctypes* :test #'string= :key #'(lambda (rec)
                                                                     (getf rec :name))))
           (szk  (find-if #'identity (getf type :szk) :key #'(lambda (rec)
                                                               (funcall (first rec) xarray)))))
      (when szk
        (append (list (getf type :dir))
                (cdr szk))))))


(defun doctemplate (xarray)
  (destructuring-bind (&optional subdir szk template)
      (select-doctype xarray)
    (declare (ignore szk))
    (when template
      (concatenate 'string
                   *doc-template-dir* "\\"
                   subdir "\\"
                   template))))


#|(defun newfile (tk ps)
  (format nil "~a~a ~a ~a ~a"
          *out-dir* tk ps
          (timestamp (get-universal-time))
          (second (find ps *templates-ps* :key #'first :test #'string=))))|#
(defun newfile (xarray)
  (destructuring-bind (&optional subdir szk template)
      (select-doctype xarray)
    (declare (ignore subdir))
    (when template
      (let ((tk (xaref xarray "Vállalat hosszú megnevezése" 1)))
        (format nil "~a~a, ~a, ~a, ~a" *out-dir* tk szk
                (timestamp (get-universal-time))
                template)))))
              

;;; ----------------------------------------------------------------------
;;; Törzs


(defparameter *page-break-needed* nil)


(defun get-fee-row (xarray row cols codes)
  (let ((width (array-dimension xarray 1)))
    (apply #'append
           (mapcar #'(lambda (col code)
                       (list code (when (< col width)
                                    (xaref xarray col row))))
                   cols codes))))


(defun get-fees (xarray)
  (let ((result '())
        (height  (array-dimension xarray 0))
        (codes  '(:code :name :sum :start :end)))
    (loop for row from 1 below height doing
          (push (get-fee-row xarray row '(15 16 17 35 29) codes) result)
          (push (get-fee-row xarray row '(19 20 21 34 30) codes) result))
    (remove-if #'(lambda (elem)
                   (string= "" (getf elem :code)))
               (remove-duplicates result :test #'equalp))))


(defun find-fee (code fees)
  (find-if #'(lambda (record)
               (string= code (getf record :code))) fees))


(defun fee-name (code refs)
  (let ((found (find-if #'(lambda (ref)
                            (member code (getf (getf ref :meta) :code) :test #'string=))
                        refs)))
    (when found
      (getf found :name))))


(defun sort-fees (fees order)
  (let ((result '()))
    (dolist (code order)
      (let ((found (find-fee code fees)))
        (when found
          (push found result))))
    (nreverse result)))


(defmacro vals-fn (binds &body body)
  (let ((clauses   '())
        ;; Ha megadtunk :FEES-t, a BODY-ból kihagyjuk
        (body-only (if (eq (first body) :fees)
                     (cddr body)
                     body))
        ;; Ha megadtunk :FEES-t, belevesszük a LET-be
        (fees      (when (eq (first body) :fees)
                     (second body))))
    (dolist (pair binds)
      (destructuring-bind (&optional symbol xref)
          pair
        (when (and symbol xref)
          (push (list symbol `(xaref xarray ,xref 1)) clauses))))
    (when fees
      (push (list fees '(get-fees xarray)) clauses))
    `(lambda (xarray)
       (let ,clauses
         ,@body-only))))


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


(defconstant +wd-header-footer-first-page+ 2)


(defparameter *t2*
  `(("Iktatószám: …………………………^M^M$………………$^M"
     ,(vals-fn ((a "Név"))
        (clean-name a)))
    
    ("Születési neve: $………………$^M"
     ,(vals-fn ((a "Születési vezetéknév") (b "Születési utónév") (c "2.születési utónév"))
        (clean-name (conc-with-single-spaces (list a b c)))))
    
    ("Születési helye, ideje: $………………$^M"
     ,(vals-fn ((a "Születési hely") (b "Születési dátum"))
        (concatenate 'string (clean-name a) ", " (excel-date-string b :words t))))

    ("Anyja neve: $………………$^M"
     ,(vals-fn ((a "Anya") (b "Anyja keresztneve") (c "Anyja 2.keresztneve"))
        (clean-name (conc-with-single-spaces (list a b c)))))

    ("Munkaköre: $………………$"
     ,(vals-fn ((a "Munkakör"))
        (remove-double-spaces
         (trim-edge-spaces a))))

    ("Munkavégzésének helye: $……………………………………………………$, cím"
     ,(vals-fn ((a "szervezeti egys hosszú megnev."))
        (remove-double-spaces 
         (trim-edge-spaces a))))

    ("Heti munkaideje: $……$ óra"
     ,(vals-fn ((a "Heti óra"))
        (round a)))

    ("FEOR száma: $………………$^M"
     ,(vals-fn ((a "FEOR-sz.s."))
        (remove-double-spaces 
         (trim-edge-spaces a))))

    ("besorolom Önt $………………$ fokozatba. ^M"
     ,(vals-fn ((a "Bérrendsz. csop név"))
        (remove-double-spaces 
         (trim-edge-spaces a))))

    ("Önt $………………$ napjától"
     ,(vals-fn ((a "Belépés dátuma"))
        (excel-date-string a :words t)))
    
    ("napjától $………………$ Tankerületi Központ"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (add-article
         (first (split-into-words a)))))

    ("(1) bekezdése alapján $$kinevezem Önt"
     ,(vals-fn ((a "Kinevezés/szerzõdés jellege"))
        (if (string= a "Határozatlan id.kine")
          ""
          "és 40. § (1)-(3) bekezdése ")))

    ("állományába $………………$ köznevelési"
     ,(vals-fn ((a "Kinevezés/szerzõdés jellege") (b "Szerz.vége"))
        (if (string= a "Határozatlan id.kine")
          "határozatlan idejû"
          (format nil "tartósan távollévõ helyettesítése céljából határozott ideig, várhatóan ~a napjáig tartó" (excel-date-string b :words t)))))

    ("illetékes törvényszékhez. ^M^M$………………$,"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (second (get-tk-data a))))

    ("Pénzügyileg ellenjegyzem. ^M^M$………………$, "
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (second (get-tk-data a))))

    ("$………………$^Mtankerületi igazgató^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (third (get-tk-data a))))

    ("$………………$^Mtitulus^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (fourth (get-tk-data a))))
    
    (,(format nil "~C~C$………………$^M~C          köznevelési foglalkoztatotti" #\tab #\tab #\tab)
     ,(vals-fn ((a "Név"))
        (clean-name a)))

    ("Székhelye: $………………$^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (sixth (get-tk-data a)))
     ,#'(lambda (doc)
          (ccom::footer doc 1 +wd-header-footer-first-page+)))

    ("Törzskönyvi azonosító szám: $………………$^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (round (fifth (get-tk-data a))))
     ,#'(lambda (doc)
          (ccom::footer doc 1 +wd-header-footer-first-page+)))

    ("$………………$^MTANKERÜLETI^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (hupcase (first (split-into-words (first (get-tk-data a))))))
     ,#'(lambda (doc)
          (ccom::header doc 1 +wd-header-footer-first-page+)))

    ("$A …-jogszabály-… alapján próbaidõ nem köthetõ ki.$^M"
     ,(vals-fn ((szk "SZK") (bd "Belépés dátuma") (pv "Próbaidõ  vége"))
        (let* ((pv-str (if (numberp pv) (excel-date-string pv :words t) "..."))
               (period (concatenate 'string (excel-date-string bd :words t) " napjától " pv-str))
               (b1-prob (concatenate 'string "A munka törvénykönyvérõl szóló 2012. évi I. törvény (a továbbiakban: Mt.) 45. § (5) bekezdése alapján a felek " period "napjáig terjedõ próbaidõt kötnek ki, amely idõtartam alatt a munkaviszonyt az Mt. 79. § (1) bekezdésének a) pontja alapján bármelyik fél azonnali hatályú felmondással – indokolás nélkül – megszüntetheti."))
               (bx-prob (concatenate 'string "A Púétv. 41. § (1) bekezdése alapján " period " napjáig tartó próbaidõt kötök ki, amely idõtartam alatt a köznevelési foglalkoztatotti jogviszonyt a Púétv. 41. § (4) bekezdése és 46. § (2) bekezdésének a) pontja alapján bármelyik fél indokolás nélkül azonnali hatállyal megszüntetheti.")))
          (if (string= szk "B1")
            (if (empty-cell-p pv)
              ""
              b1-prob)
            ;; Egyéb személyi körök
            (if (empty-cell-p pv)
              "A …-jogszabály-… alapján próbaidõ nem köthetõ ki."
              bx-prob)))))

    ("$A pedagógusok új életpályájáról szóló 2023. évi LII. törvény végrehajtásáról szóló 401/2023. (VIII. 30.) Korm. rendelet (a továbbiakban: Púétv. vhr.) 37. § (1)-(13) bekezdése alapján az Ön gyakornoki ideje ……………… napjától ……………… napjáig tart, minõsítõ vizsgát ……………… napjáig köteles tenni. Amennyiben a minõsítõ vizsgája sikeres, a Púétv. vhr. 37. § (8) bekezdése alapján Önt ……………… fokozatba kell besorolni.$^M"
     ,(vals-fn ((besor "Bérrendsz. csop név") (bd "Belépés dátuma") (vh "Határidõ"))
        (let ((bd-str (excel-date-string bd :words t))
              (vh-str (if (empty-cell-p vh)
                        (excel-date-string vh :words t)
                        "...")))
          (if (string= besor "Gyakornok")
            (concatenate 'string "A  pedagógusok új életpályájáról szóló 2023. évi LII. törvény végrehajtásáról szóló 401/2023. (VIII. 30.) Korm. rendelet (a továbbiakban: Púétv. vhr.) 37. § (1)-(13) bekezdése alapján az Ön gyakornoki ideje " bd-str " napjától ...  napjáig tart, minõsítõ vizsgát " vh-str " napjáig köteles tenni. Amennyiben a minõsítõ vizsgája sikeres, a Púétv. vhr. 37. § (8) bekezdése alapján Önt Pedagógus I. fokozatba kell besorolni.")
            ""))))

    ("hatállyal $………………$ alapján az alábbiak szerint állapítom meg.^M"
     ,(vals-fn ((szk "SZK") (bes "Bérrendsz. csop név") (eila "Esélyteremtési illetményrészre")) :fees fees
        (let ((cref::*coderefs*  cref::*puetv-b1b2b8b9-illetmenyelemek-2024*)
              (cref::*codenames* cref::*puetv-megnevezes-2024*))
          (let* ((codes (mapcar #'(lambda (fee) (getf fee :code)) fees))
                 (fees  (cref::fees :codes codes :ps szk :lab bes :eila eila))
                 (text  (cref::convert fees)))
            text))))

    ("Havi illetményét $………………$ napi hatállyal"
     ,(vals-fn ((a "Belépés dátuma"))
        (excel-date-string a :words t)))

    (,(format nil "$Havi illetmény:~C………………~CFt^MIlletmény összesen:~C………………~cFt$^M" #\tab #\tab #\tab #\tab)
     ,(vals-fn ((bd "Belépés dátuma")) :fees fees
        (let* ((ordered (sort-fees fees cref::*puetv-b1b2b8b9-illetmenyelemek-2024-sorrend*))
               (total   0)
               (digest  (mapcar #'(lambda (fee)
                                    (destructuring-bind (&key code name sum start end) fee
                                      (declare (ignore name))
                                      (incf total sum)
                                      (append
                                       (list (fee-name code cref::*puetv-b1b2b8b9-illetmenyelemek-2024*)
                                             (currency sum))
                                       (when (string/= code "1P00")
                                         (list
                                          (excel-date-string (or start bd) :words t)))
                                       (when (and (string/= code "1P00")
                                                  end
                                                  (/= end 2958465))
                                         (list
                                          (excel-date-string end :words t))))))
                                ordered))
               (lines  '()))
          (dolist (cookin digest)
            (destructuring-bind (name sum &optional start end) cookin
              (push (format nil "~a:~C~a~CFt~C" name #\tab sum #\tab #\return) lines)
              (when start
                (if end
                  (push (format nil "megállapításának idõszaka: ~a napjától ~a napjáig~C" start end #\return) lines)
                  (push (format nil "megállapításának idõszaka: ~a napjától~C" start #\return) lines)))))
          (push (format nil "Illetmény összesen:~C~a~CFt~C" #\tab (currency total) #\tab #\return) lines)
          (apply #'concatenate 'string
                 (nreverse lines)))))

#|    ("$23$"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (third (get-tk-data a))))

    ("$24$"
     ,(vals-fn () :fees fees
        (currency (getf (find-fee "1100" fees) :sum))));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ("$25$"
     ,(vals-fn () :fees fees
        (let ((sum (getf (find-fee "1100" fees) :sum)));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (when sum
            (sub->words sum)))))|#
))


(defun temp-target (temp)
  (let* ((external (ccom::carriage-return temp))
         (start    (position #\$ external))
         (end      (position #\$ external :from-end t))
         (clean    (concatenate
                    'string
                    (subseq external 0 start)
                    (subseq external (1+ start) end)
                    (subseq external (1+ end)))))
    (values clean start (1- end))))


(defun fill-template (current xarray)
  (dolist (desc *t2*)
    (destructuring-bind (temp val-fn &optional range-fn)
        desc
      (multiple-value-bind (clean start-offset end-offset)
          (temp-target temp)
        (cclet* ((range  (if range-fn
                           (funcall range-fn current)
                           #p(content current)))
                 (found  (ccom::range-find-text range clean)))
          (when found
            (cclet* ((start (+ found start-offset))
                     (end   (+ found end-offset))
                     (text  (format nil "~a" (funcall val-fn xarray))))
              (ccom::selection-overwrite range start end text))))))))


(defconstant +wd-section-break-next-page+ 2)


(defun add-template (doc xarray)
  ;; Új temp file dok.sablon alapján
  (let ((temp-name (doctemplate xarray)))
    (when temp-name
      (with-document (current :open-file temp-name :read-only t :close t)
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
      ;; Eredmény állapotának mentése
      #m(save doc)
      t)))


(defun line (n)
  (format nil "~v@{~A~:*~}" n "-"))


(defun process ()
  (with-workbook (wbook :open-file *xls-query* :read-only t :wsvars (ws-query) :close t)
    (let ((tk-head "Vállalat hosszú megnevezése"))
      ;; Progress bar
      (with-progress ("Dokumentumok generálása" move dump (length (xcol-uniques ws-query "SZTSZ")))
        ;; Iteráció TK-kon.
        (xdouniq (tk ws-query tk-head)
          (dump (format nil "~%~a~%~a~%~a~%~%" (line 70) tk (line 70)))
          ;; Iteráció személyi körökön.
          (xdouniq (ps ws-query "SZK" :select `((,tk-head ,tk)))
            (dump (format nil "~a~%~a személyi kör~%~%" (line 35) ps))
            ;; Új dokumentum létrehozása, mentés másként
            (let ((album-name (newfile #p(value2 (used-range 
                                                  (xselect> ws-query
                                                            `((,tk-head ,tk) ("SZK" ,ps))))))))
              ;; Ha jelen személyi körhöz nincs definiálva doctype:
              (if (not album-name)
                (progn
                  (dump (format nil "~a személyi kör nincs definiálva.~%" ps))
                  (xdouniq (sztsz ws-query "SZTSZ" :select `((,tk-head ,tk) ("SZK" ,ps)))
                    (dump (format nil "SZTSZ: ~a   kihagyva~%" sztsz))
                    (move)))
                ;; Ha van:
                (with-document (album :close t :save t)
                  #m(saveas2 album album-name)
                  ;; Oldaltörés inicializálása.
                  (setf *page-break-needed* nil)
                  ;; Iteráció SZTSZ-eken:
                  (xdouniq (sztsz ws-query "SZTSZ" :select `((,tk-head ,tk) ("SZK" ,ps)))
                    (dump (format nil "SZTSZ: ~a" sztsz))
                    ;; SZTSZ adatainak beírása a dokumentumba.
                    (if (add-template album
                                      #p(value2 (used-range (xselect> ws-query
                                                                      `(("SZTSZ" ,sztsz))))))
                      (dump (format nil "   ok~%"))
                      (dump (format nil "   HIBA!~%")))
                    (move)))))))
        (dump (format nil "~%~%~%~%"))))))


;;; ----------------------------------------------------------------------
;;; Main


(defun save-state ()
  (save-forms
   (appfile "state.txt")
   `(:doctype ,*doctype*
     :query1  ,*xls-query*
     :query2  ,*xls-query2*
     :tempdir ,*doc-template-dir*
     :outdir  ,*out-dir*)))


(defun load-state ()
  (let ((state (first (load-forms "state.txt"))))
    (when state
      (destructuring-bind (&key doctype query1 query2 tempdir outdir &allow-other-keys)
          state
        (setf *doctype*          doctype
              *xls-query*        query1
              *xls-query2*       query2
              *doc-template-dir* tempdir
              *out-dir*          outdir
              *xls-tks*          (namestring (merge-pathnames *xls-tks-filename* tempdir)))))))


(defun init-state ()
  (setf *doctype*          (getf (first *doctypes*) :name)
        *xls-query*        (appdir)
        *xls-query2*       ""
        *doc-template-dir* (appdir)
        *out-dir*          (appdir)
        *xls-tks*          (namestring (merge-pathnames *xls-tks-filename* (appdir)))))


(defun start ()
  (init-state)
  (load-state)
  (wg-window
   "Kinevezés-generáló"
   (wg-options "Dokumentumtípus választása"
               #'(lambda (text &rest rest)
                   (setf *doctype* text))
               (mapcar #'(lambda (rec)
                           (getf rec :name))
                       *doctypes*))
   (wg-file-selector "SAP lekérdezés eredménye"
                     "*.xlsx"
                     '("Excel fájlok" "*.xlsx" "Minden fájl" "*.*")
                     #'(lambda (text &rest rest)
                         (setf *xls-query* text))
                     *xls-query*)
   (wg-dir-selector "Dokumentumsablonok mappája"
                    #'(lambda (text &rest rest)
                        (setf *doc-template-dir* text
                              *xls-tks*          (namestring (merge-pathnames *xls-tks-filename* text ))))
                    *doc-template-dir*)
   (wg-dir-selector "Generált dokumentumok mappája"
                    #'(lambda (text &rest rest)
                        (setf *out-dir* text))
                    *out-dir*)
   (wg-button "Dokumentumok generálása"
              #'(lambda (if)
                  (save-state)
                  (process)))))


;;; ----------------------------------------------------------------------
;;; Main


(defun test ()
  (let (        
;        (*xls-query*        "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\wax-EXPORT_orig.XLSX")
        (*xls-query*        "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\wax-EXPORT.XLSX")
        (*doc-template-dir* "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Dokumentumsablonok\\")
        (*out-dir*          "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\Eredmény\\")
        (*xls-tks*          "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\TK vezetõk.xlsx"))
    (save-state)
    (process)))


(defun test01 ()
  (with-document (doc :open-file "c:\\Users\\cselovszkid\\common-lisp\\wax\\Munka\\sandbox\\bookmarks.docx")
    #p(text #p(content doc))))
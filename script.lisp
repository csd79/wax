;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;;; ----------------------------------------------------------------------
;;; Globális változók


(defparameter *xls-tks-filename* "TK vezetõk.xlsx")

(defparameter *xls-query*        "")
(defparameter *xls-query2*       "")
(defparameter *kir-wbook*        "")
(defparameter *fileno-wbook*     "")
(defparameter *doc-template-dir* "")
(defparameter *out-dir*          "")
(defparameter *xls-tks*          "")

(defparameter *doctype*          "")

(defparameter *school-year-end*  45900) ; 2025.08.31. Ha a felületen lenne, átírnák hülyeséggel.

(defparameter *mod-start-def*    "2025. január 1.")
(defparameter *mod-start*        *mod-start-def*)


;;; ----------------------------------------------------------------------
;;; Generálható dokumentumtípusok


(defun szk-fn (szk)
  #'(lambda (xarray)
      (string= (xcref xarray "SZK") szk)))

(defun b1-noks-fn ()
  #'(lambda (xarray)
      (and (string= (xcref xarray "SZK") "B1")
;           (member (xcref xarray "Munkakör") '("4336" "4211" "4214") :test #'string=))))
           (member (xcref xarray :x) '("4336" "4211" "4214") :test #'string=))))

(defun b1-kiseg-fn ()
  #'(lambda (xarray)
      (and (string= (xcref xarray "SZK") "B1")
;           (not (member (xcref xarray "Munkakör") '("4336" "4211" "4214") :test #'string=)))))
           (not (member (xcref xarray :x) '("4336" "4211" "4214") :test #'string=)))))

(defparameter *doctypes*
  `(
    (:name "Kinevezések"
     :dir  "Kinevezések"
     :szk
     ((,(szk-fn "B2") "B2" "Pedagógus_kinevezési okmány.docx")
      (,(szk-fn "B8") "B8" "Ped szakkép_noks_Púétv_kinevezési okmány.docx")
      (,(szk-fn "B9") "B9" "Nem ped szakkép_noks_Púétv_kinevezési okmány.docx")
      (,(b1-noks-fn)  "B1" "Munkaszerz._noks munkakör_munkavállaló.docx")
      (,(b1-kiseg-fn) "B1" "Munkaszerz._gazd., ügyv., mûsz.,kiseg.munkakör_munkavállaló.docx")))

    (:name "Egyoldalú kinevezésmódosítások"
     :dir  "Egyoldalú kinevezésmódosítások"
     :szk
     ((,(szk-fn "B2") "B2" "Kinevmód_egyoldalú_pedagógus.docx")
      (,(szk-fn "B8") "B8" "Kinevmód_egyoldalú_ped. szakkép. noks.docx")
      (,(szk-fn "B9") "B9" "Kinevmód_egyoldalú_nem ped. szakkép. noks.docx")))

    (:name "Kétoldalú kinevezésmódosítások"
     :dir  "Kétoldalú kinevezésmódosítások"
     :szk
     ((,(szk-fn "B2") "B2" "Kinevmód_kétoldalú_pedagógus.docx")
      (,(szk-fn "B8") "B8" "Kinevmód_kétoldalú_ped. szakkép. noks.docx")
      (,(szk-fn "B9") "B9" "Kinevmód_kétoldalú_nem ped. szakkép. noks.docx")
      (,(b1-noks-fn)  "B1" "Munkaszerz.mód_noks munkakör_munkavállaló.docx")
      (,(b1-kiseg-fn) "B1" "Munkaszerz.mód_gazd., ügyv., mûsz.,kiseg.munkakör_munkavállaló.docx")))))


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


(defun newfile-ungrouped (xarray)
  (destructuring-bind (&optional subdir szk template)
      (select-doctype xarray)
    (declare (ignore subdir))
    (when template
      (let* ((tk      (xcref xarray "Vállalat hosszú megnevezése"))
             (tk-ok   (remove-illegal-filename-chars (format nil "~a TK" (first (str:words tk)))))
             (name-ok (remove-illegal-filename-chars (str:replace-all " " "_" (remove-illegal-filename-chars (clean-name (xcref xarray "Név")))))))
#|        (format nil "~a~a, ~a, ~a, ~a, ~a" *out-dir* tk-ok szk name-ok
                (timestamp (get-universal-time))
                template)))))|#
        (format nil "~a~a_kinevezésmódosítás_2025_01_01" *out-dir* name-ok)))))


(defun tempfile ()
  (format nil "~a~a_~a~a"
          *out-dir* "temp" (timestamp (get-universal-time)) ".docx"))


;;; ----------------------------------------------------------------------
;;; Törzs


(defun get-fee-row (row cols codes)
  (apply #'append
         (mapcar #'(lambda (col code)
                     (list code (xcref row col)))
                 cols codes)))


(defun get-fees (xarray)
  (let ((result '())
        (codes  '(:code :name :sum :start :end :titl)))
    (do-xarows (row r xarray)
;      (push (get-fee-row row '(15 16 17 35 29) codes) result)
;      (push (get-fee-row row '(19 20 21 34 30) codes) result))
      (push (get-fee-row row '(15 16 17 36 30 39) codes) result) ; kód név összeg kezdete vége címzetes
      (push (get-fee-row row '(19 20 21 35 31 39) codes) result))
    (remove-if #'(lambda (elem)
                   (string= "" (getf elem :code)))
               (remove-duplicates result :test #'equalp))))


(defun find-fee (code fees)
  (find-if #'(lambda (record)
               (string= code (getf record :code))) fees))


(defun fee-name (code eila titl refs)
  (let ((found (find-if #'(lambda (ref)
                            (let* ((meta  (getf ref :meta))
                                   (mcode (getf meta :code))
                                   (meila (getf meta :eila))
                                   (mtitl (getf meta :titl))
                                   )
                              (and (member code mcode :test #'string=)
                                   (if meila
                                     (member eila meila :test #'string=)
                                     t)
                                   (if mtitl
                                     (member titl mtitl :test #'string=)
                                     t))))
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
      (destructuring-bind (&optional symbol column)
          pair
        (when (and symbol column)
          (push (list symbol `(xcref xarray ,column)) clauses))))
    (when fees
      (push (list fees '(get-fees xarray)) clauses))
    `(lambda (xarray)
       (let ,clauses
         ,@body-only))))


;;; ----------------------------------------------------------------------
;;; TK vezetõk adatai


(defparameter *tk-data* nil)

;;; TK vezetõ adatok
(defun tk-row (tk)
  ;; Adatok inicializálása, ha még nem történt meg
  (when (null *tk-data*)
    (setf *tk-data*
          (with-workbook (:open *xls-tks* :read-only t :wsvars (tks) :close t)
            (read-xarray (used-range tks)))))
  ;; TK sor keresése
  (xaselect *tk-data*
            #'(lambda (row)
                (astring= (xcref row "TK") tk))))


;;; ----------------------------------------------------------------------
;;; KIR felhelylista


(defparameter *kir-data* nil)

(defun parse-string (value &key (numproc #'identity) (ignore-error t))
  (typecase value
    (string value)
    (number (format nil "~d" (funcall numproc value)))
    (t      (unless ignore-error (error "Value should be of type NUMBER or STRING.")))))

(defun parse-number (value &key (strproc #'read-from-string) (ignore-error t))
  (let ((parsed (typecase value
                  (string (unless (string= value "") (funcall strproc value)))
                  (number value)
                  (t      (unless ignore-error (error "Value should be of type NUMBER or STRING."))))))
    (when (numberp parsed)
      parsed)))

(defun parse-number-as-string (value length)
  (let ((num (parse-number value)))
    (if num
      (let* ((str (format nil "~d" num))
             (len (length str)))
        (if (<= length len)
          (subseq str (- len length))
          (format nil "~v{~A~:*~}~D" (- length len) '("0") num)))
      "")))

(defun xarray-zero-index-p (xarray)
  (zerop (length (index xarray))))

(defun feh-address (kir-row &optional (not-found-value "………………"))
  (if kir-row
    (if (xarray-zero-index-p kir-row)
      not-found-value
      (format nil "~D ~a, ~a"
              (round (xcref kir-row "A feladatellátási hely irányítószáma"))
              (xcref kir-row "A feladatellátási hely települése")
              (xcref kir-row "A feladatellátási hely pontos címe")))
    not-found-value))

(defun kir-row (om fh)
  (when (string/= *kir-wbook* "")
    (unless *kir-data*
      (with-workbook (:open *kir-wbook* :wsvars (data) :read-only t)
        (cclet* ((filtered (xselect> data `(("Fenntartó típusa" "tankerületi központ"))))
                 (bottom   (last-row filtered))
                 (range    (range filtered 1 1 10 bottom)))
          (setf *kir-data* (read-xarray range)))))
    (when *kir-data*
      (xaselect *kir-data*
                #'(lambda (row)
                    (and
                     (= (parse-number om) (parse-number (xcref row "OM azonosító")))
                     (= (parse-number fh) (parse-number (xcref row "A feladatellátási hely sorszáma")))))))))
#|                     (string= (parse-number-as-string om 6)
                              (parse-number-as-string (xcref row "OM azonosító") 6))
                     (string= (parse-number-as-string fh 3)
                              (parse-number-as-string (xcref row "A feladatellátási hely sorszáma") 3))))))))|#


;;; ----------------------------------------------------------------------
;;; KIR felhelylista


(defparameter *fileno-data* nil)


(defun fileno-row (sztsz)
  (when (string/= *fileno-wbook* "")
    ;; Ha még nincs beolvasva, beolvasás
    (unless *fileno-data*
      (with-workbook (:open *fileno-wbook* :wsvars (data) :read-only t)
        (cclet* ((bottom (last-row data))
                 (range  (range data 1 1 10 bottom)))
          (setf *fileno-data* (read-xarray range)))))
    ;; Iktatószám keresése
    (when *fileno-data*
      (xaselect *fileno-data*
                #'(lambda (row)
                    (= (parse-number sztsz) (parse-number (xcref row "SZTSZ"))))))))
#|                    (string= (parse-number-as-string sztsz 8)
                             (parse-number-as-string (xcref row "SZTSZ") 8)))))))|#


;;; ----------------------------------------------------------------------
;;; Átírások


(defparameter *t2*
  `(
    ("Iktatószám: $………………$^M"
     ,(vals-fn ((sztsz "SZTSZ"))
        (let ((row (fileno-row sztsz)))
          (if (and row
                   (not (zerop (xarray-indexed-height row))))
            (xcref row "POSZEIDON iktatószám")
            "………………"))))

    ("$………………$^MTANKERÜLETI^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (astring-upcase (first (str:words (xcref (tk-row a) "TK")))))
     ,#'(lambda (doc)
          (ccom::header doc 1 +wd-header-footer-first-page+)))

    ("Székhelye: $………………$^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "Székhely"))
     ,#'(lambda (doc)
          (ccom::footer doc 1 +wd-header-footer-first-page+)))

    ("Törzskönyvi azonosító szám: $………………$^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (round (xcref (tk-row a) "Törzsszám")))
     ,#'(lambda (doc)
          (ccom::footer doc 1 +wd-header-footer-first-page+)))
    
    ("$………………$^Mfoglalkoztatott részére"
     ,(vals-fn ((a "Név"))
        (clean-name a)))
    
    (,(format nil "Születési neve:~C$………………$^M" #\tab)
     ,(vals-fn ((a "Születési vezetéknév") (b "Születési utónév") (c "2.születési utónév"))
        (clean-name (str:unwords (list a b c)))))
    
    (,(format nil "Születési helye, ideje:~C$………………$^M" #\tab)
     ,(vals-fn ((a "Születési hely") (b "Születési dátum"))
        (concatenate 'string (clean-city a) ", " (excel-date-string b :words t))))

    (,(format nil "Anyja neve:~C$………………$^M" #\tab)
     ,(vals-fn ((a "Anya") (b "Anyja keresztneve") (c "Anyja 2.keresztneve"))
        (clean-name (str:unwords (list a b c)))))

    ("(1) bekezdése $$alapján kinevezem Önt"
     ,(vals-fn ((a "Kinevezés/szerzõdés jellege"))
        (if (string= a "Határozatlan id.kine")
          ""
          "és 40. § (1)-(3) bekezdése ")))

    ("Önt $………………$ napjától"
     ,(vals-fn ((a "Belépés dátuma"))
        (excel-date-string a :words t)))
    
    ("napjától $………………$ Tankerületi Központ"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (add-article
         (first (str:words a)))))

    ("állományába $………………$ köznevelési"
     ,(vals-fn ((a "Kinevezés/szerzõdés jellege") (b "Szerz.vége") (c "Hely.dolg.neve."))
        (if (string= a "Határozatlan id.kine")
          "határozatlan idejû"
          (format nil "~a tartósan távollévõ helyettesítése céljából határozott ideig, várhatóan ~a napjáig tartó"
                  (if (empty-cell-p c)
                    "………………"
                    (astring-capitalize c))
                  (if (empty-cell-p b)
                    "………………"
                    (excel-date-string b :words t))))))

    ("$A …-jogszabály-… alapján próbaidõ nem köthetõ ki.$^M"
     ,(vals-fn ((szk "SZK") (bd "Belépés dátuma") (pv "Próbaidõ  vége"))
        (let* ((pv-str (if (empty-cell-p pv)
                         "………"
                         (excel-date-string pv :words t)))
               (period (concatenate 'string (excel-date-string bd :words t) " napjától " pv-str))
               (b1-prob (concatenate 'string "A munka törvénykönyvérõl szóló 2012. évi I. törvény (a továbbiakban: Mt.) 45. § (5) bekezdése alapján a felek " period " napjáig terjedõ próbaidõt kötnek ki, amely idõtartam alatt a munkaviszonyt az Mt. 79. § (1) bekezdésének a) pontja alapján bármelyik fél azonnali hatályú felmondással – indokolás nélkül – megszüntetheti."))
               (bx-prob (concatenate 'string "A Púétv. 41. § (1) bekezdése alapján " period " napjáig tartó próbaidõt kötök ki, amely idõtartam alatt a köznevelési foglalkoztatotti jogviszonyt a Púétv. 41. § (4) bekezdése és 46. § (2) bekezdésének a) pontja alapján bármelyik fél indokolás nélkül azonnali hatállyal megszüntetheti.")))
          (if (string= szk "B1")
            (if (empty-cell-p pv)
              ""
              b1-prob)
            ;; Egyéb személyi körök
            (if (empty-cell-p pv)
              "A …-jogszabály-… alapján próbaidõ nem köthetõ ki."
              bx-prob)))))

    (,(format nil "unkaköre:~C$………………$^M" #\tab)
;     ,(vals-fn ((a "Munkakör"))
     ,(vals-fn ((a :y))
        (str:unwords (str:words
         (str:trim a)))))

    (,(format nil "Munkavégzésének helye:~C$……………………………………………………$" #\tab)
     ,(vals-fn ((a "szervezeti egys hosszú megnev."))
        (str:unwords (str:words
         (str:trim a)))))

    (", $cím$^Mvagy^MMunkavégzés"
     ,(vals-fn ((a "Szervezeti egység OM azonosító") (b "Szerv.egység feladat ellát hel"))
        (if (and a b (string/= *kir-wbook* ""))
          (feh-address (kir-row a b) "cím")
          "cím")))
    
    (,(format nil "Heti munkaideje:~C$……$ óra" #\tab)
     ,(vals-fn ((a "Heti óra"))
        (if (= a (round a))
          (format nil "~d" (round a))
          (str:replace-first "." "," (format nil "~,2f" a)))))

    ("óra $teljes munkaidõ/$részmunkaidõ/csökkentett munkaidõ^MFEOR"
     ,(vals-fn ((a "Heti óra"))
        (if (= a 40)
          "teljes munkaidõ "
          "")))

    ("óra $teljes munkaidõ/$részmunkaidõ^MFEOR"
     ,(vals-fn ((a "Heti óra"))
        (if (= a 40)
          "teljes munkaidõ "
          "")))

    ("$részmunkaidõ/csökkentett munkaidõ$^MFEOR"
     ,(vals-fn ((a "Heti óra"))
        (if (= a 40)
          ""
          "részmunkaidõ/csökkentett munkaidõ")))

    ("$részmunkaidõ$^MFEOR"
     ,(vals-fn ((a "Heti óra"))
        (if (= a 40)
          ""
          "részmunkaidõ")))

    (,(format nil "FEOR száma:~C$………$^M" #\tab)
     ,(vals-fn ((a "FEOR-sz.s."))
        (str:unwords (str:words
         (str:trim a)))))

    ("besorolom Önt $………………$ fokozatba. ^M"
     ,(vals-fn ((a "Bérrendsz. csop név"))
        (str:unwords (str:words
         (str:trim a)))))

    ("$^MA pedagógusok új életpályájáról szóló 2023. évi LII. törvény végrehajtásáról szóló 401/2023. (VIII. 30.) Korm. rendelet (a továbbiakban: Púétv. vhr.) 37. § (1)-(13) bekezdése alapján az Ön gyakornoki ideje ……………… napjától ……………… napjáig tart, minõsítõ vizsgát ……………… napjáig köteles tenni. Amennyiben a minõsítõ vizsgája sikeres, a Púétv. vhr. 37. § (8) bekezdése alapján Önt ……………… fokozatba kell besorolni.^M^M$"
     ,(vals-fn ((besor "Bérrendsz. csop név") (bd "Belépés dátuma") (vh "Határidõ"))
        (let ((bd-str (excel-date-string bd :words t))
              (vh-str (if (empty-cell-p vh)
                        "………………"
                        (excel-date-string vh :words t))))
          (if (string= besor "Gyakornok")
            (concatenate 'string "^MA  pedagógusok új életpályájáról szóló 2023. évi LII. törvény végrehajtásáról szóló 401/2023. (VIII. 30.) Korm. rendelet (a továbbiakban: Púétv. vhr.) 37. § (1)-(13) bekezdése alapján az Ön gyakornoki ideje " bd-str " napjától ………………  napjáig tart, minõsítõ vizsgát " vh-str " napjáig köteles tenni. Amennyiben a minõsítõ vizsgája sikeres, a Púétv. vhr. 37. § (8) bekezdése alapján Önt Pedagógus I. fokozatba kell besorolni.^M^M")
            ""))))

    ("módosítom.^M^MHavi illetményét $………………$ napi hatállyal" ; Egyoldalú
     ,(vals-fn ()
        (declare (ignore xarray))
        *mod-start*))

    ("Havi illetményét $………………$ napi hatállyal" ; Kétoldalú
     ,(vals-fn ()
        (declare (ignore xarray))
        *mod-start*))

    (" heti munkaidejére tekintettel – $………………$ alapján az alábbiak szerint állapítom meg.^M"
     ,(vals-fn ((szk "SZK") (bes "Bérrendsz. csop név") (eila "Esélyteremtési illetményrészre")
                (titl "CÍm")) :fees fees
        (let ((cref::*coderefs*  cref::*puetv-b1b2b8b9-illetmenyelemek-2025jan*)
              (cref::*codenames* cref::*puetv-megnevezes-2025jan*)
#|              (cref::*defined-tvs* (if (string= bes "Gyakornok")
                                     '("1puetv" "2puetv-vhr")
                                     '("1puetv"))))|#
              (cref::*defined-tvs* '("1puetv")))
          (let* ((codes (mapcar #'(lambda (fee) (getf fee :code)) fees))
                 (fees  (cref::fees :codes codes :ps szk :lab bes :eila eila :titl titl))
                 (text  (cref::convert fees)))
            text))))

#|    ("rendelet 3. § (3) bekezdésére tekintettel – $………………$ alapján az alábbiak"
     ,(vals-fn ((szk "SZK") (bes "Bérrendsz. csop név") (eila "Esélyteremtési illetményrészre")
                (titl "CÍm")) :fees fees
        (let ((cref::*coderefs*  cref::*puetv-b1b2b8b9-illetmenyelemek-2025jan*)
              (cref::*codenames* cref::*puetv-megnevezes-2025jan*)
              (cref::*defined-tvs* (if (string= bes "Gyakornok")
                                     '("1puetv" "2puetv-vhr")
                                     '("1puetv"))))
          (let* ((codes (mapcar #'(lambda (fee) (getf fee :code)) fees))
                 (fees  (cref::fees :codes codes :ps szk :lab bes :eila eila :titl titl))
                 (text  (cref::convert fees)))
            text))))|#

    (,(format nil "$Havi illetmény:~C………………~CFt^MIlletmény összesen:~C………………~cFt$^M" #\tab #\tab #\tab #\tab)
     ,(vals-fn ((bd "Belépés dátuma") (hiv "Szerz.vége") (eila "Esélyteremtési illetményrészre")) :fees fees
;        (let* ((ordered (sort-fees fees cref::*puetv-b1b2b8b9-illetmenyelemek-2024-sorrend*))
        (let* ((ordered (sort-fees fees cref::*puetv-b1b2b8b9-illetmenyelemek-2025jan-sorrend*))
               (total   0)
               (digest  (mapcar #'(lambda (fee)
                                    (destructuring-bind (&key code name sum start end titl) fee
                                      (declare (ignore name))
                                      (incf total sum)
                                      (append
                                       ;; Ill.e. megnevezés
;                                       (list (fee-name code eila cref::*puetv-b1b2b8b9-illetmenyelemek-2024*)
                                       (list (fee-name code eila titl cref::*puetv-b1b2b8b9-illetmenyelemek-2025jan*)
                                             ;; Összeg
                                             (currency sum))
                                       ;; Megállapítás idõszak kezdete:
                                       ;;   Havi ill. vagy mesterfok: nem kell feltüntetni.
                                       (cond ((member code '("1P00" "1116") :test #'string=)
                                              nil)
                                             ;; Esélyteremtési: balépés dátuma vagy tanévkezdet (amelyik késõbbi)
                                             ((member code '("1114" "1115") :test #'string=)
                                              (list
                                               (if (> (hudate->unitime (excel-date bd))
                                                      (hudate->unitime (parse-hudate *mod-start*)))
                                                 (excel-date-string bd :words t)
                                                 *mod-start*)))
                                             ;; Pályakezdés után járó illetménynövekedés
                                             ((string= code "1113")
                                              (list "2025. január 1."))
                                             ;; Egyébként: ill.érvényesség kezdete, vagy ha nincs, belépés dátuma.
                                             (t (list (excel-date-string (or start bd) :words t))))
                                       ;; Vége dátum:
                                       ;;   Esélyteremtési:
                                       ;;     ha határozott idõ vége meg van adva és kisebb mint tanév vége:
                                       ;;       hat.idõ vége
                                       ;;     különben:
                                       ;;       tanév vége
                                       (cond ((member code '("1114" "1115") :test #'string=)
                                              (list (excel-date-string
                                                     (if (empty-cell-p hiv)
                                                       *school-year-end*
                                                       (min *school-year-end* hiv))
                                                     :words t)))
                                             ;; Pályakezdés után járó illetménynövekedés
                                             ((string= code "1113")
                                              (list (excel-date-string
                                                     (if (empty-cell-p hiv)
                                                       46022 ; 2025.12.31.
                                                       (min 46022 hiv))
                                                     :words t)))
                                             ;; Egyébként, ha nem havi illetmény, és az ill.érv.vége
                                             ;;   meg van adva és nem 9999.12.31:
                                             ;;     ill.érv. vége
                                             ((and (string/= code "1P00")
                                                   (not (empty-cell-p end))
                                                   (/= end 2958465))
                                              (list (excel-date-string end :words t)))
                                             ;; Egyébként: nem kell feltüntetni.
                                             (t nil)))))
                                ordered))
               (lines  '()))
          (dolist (cookin digest)
            (destructuring-bind (name sum &optional start end) cookin
              (push (format nil "~a:~C~a~CFt~C" name #\tab sum #\tab #\return) lines)
              (when start
                (if end
                  (push (format nil "megállapításának idõszaka: ~a napjától ~a napjáig~C" start end #\return) lines)
                  (push (format nil "megállapításának idõszaka: ~a napjától~C" start #\return) lines)))))
          (push (format nil "Illetmény összesen:~C~a~CFt" #\tab (currency total) #\tab) lines)
          (apply #'concatenate 'string
                 (nreverse lines)))))

    ("illetékes törvényszékhez.^M^M$………………$,"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "Helységnév")))

    ("Pénzügyileg ellenjegyzem.^M^M$………………$,"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (xcref (tk-row a) "Helységnév")))
        
    ("$………………$^Mtankerületi igazgató^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "TK ig")))
        
    ("$………………$^Mgazdasági vezetõ^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "Gazdasági vez.")))

    ("$NÉV$^Mtankerületi igazgató^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "TK ig")))
        
    ("$NÉV$^Mgazdasági vezetõ^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "Gazdasági vez.")))
    
    (,(format nil "~C$………………$^M~Cköznevelési foglalkoztatotti" #\tab #\tab)
     ,(vals-fn ((a "Név"))
        (clean-name a)))

    ("egyrészrõl $………………$ Tankerületi Központ"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (add-article
         (first (str:words a)))))

    ("(székhelye: $………………$, t"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "Székhely")))

    ("nyvi azonosító szám: $………………$, ké"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (round (xcref (tk-row a) "Törzsszám"))))

    (", képviseli: $………………$ tankerületi igazgató"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "TK ig")))

    (", másrészrõl $………………$ (szül"
     ,(vals-fn ((a "Név"))
        (clean-name a)))
    
    (" (születési neve: $………………$, szül"
     ,(vals-fn ((a "Születési vezetéknév") (b "Születési utónév") (c "2.születési utónév"))
        (clean-name (str:unwords (list a b c)))))
    
    (", születési helye és ideje: $………………$, any"
     ,(vals-fn ((a "Születési hely") (b "Születési dátum"))
        (concatenate 'string (clean-name a) ", " (excel-date-string b :words t))))

    (", anyja neve: $………………$) mint"
     ,(vals-fn ((a "Anya") (b "Anyja keresztneve") (c "Anyja 2.keresztneve"))
        (clean-name (str:unwords (list a b c)))))

    ("A munkáltató a munkavállalót $………………$ napjától"
     ,(vals-fn ((a "Belépés dátuma"))
        (excel-date-string a :words t)))

    ("napjától $………………$ munkaviszony keretében"
     ,(vals-fn ((a "Kinevezés/szerzõdés jellege")
                (c "Hely.dolg.neve.") (d "Szerz.vége"))
        (cond
         ;; Határozatlan idejû kinevezés/szerzõdés
         ((member a '("Határozatlan id.kine" "Hatlan. idejû MT sz.") :test #'string=)
          "határozatlan idejû")
         ;; Határozott idejû helyettesítõ
         ((notany #'empty-cell-p (list c d))
          (format nil "~a tartósan távollévõ helyettesítése céljából határozott ideig, várhatóan ~a napjáig tartó"
                  ;; Helyettesített dolgozó
                  (if (string/= c "")
                    (astring-capitalize c)
                    "………………")
                  ;; Szerzõdés vége
                  (excel-date-string d :words t)))
         ;; Határozott idejû nem-helyettesítõ
         ((not (empty-cell-p d))
          (format nil "határozott ideig, ~a napjáig tartó"
                  (excel-date-string d :words t)))
         ;; Nem meghatározható eset
         (t "napjától $………………$ munkaviszony keretében"))))
    
    (,(format nil "^MMunkavégzés helye:~C$……………………………………………………$" #\tab)
     ,(vals-fn ((a "szervezeti egys hosszú megnev."))
        (str:unwords (str:words
         (str:trim a)))))

    ("munkavállaló havi bruttó alapbére $……………… Ft, azaz ………………$ forint."
     ,(vals-fn () :fees fees
        (declare (ignore xarray))
        (destructuring-bind (&key code name sum start end)
            (first fees)
          (declare (ignore code name start end))
          (format nil "~a Ft, azaz ~a"
                  (currency sum)
                  (sub->words sum)))))

    (,(format nil "^M~C$………………$, 2025" #\tab)
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "Helységnév")))

    (,(format nil "Pénzügyileg ellenjegyzem.^M^M~C$………………$" #\tab)
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "Helységnév")))

    (,(format nil "~C$………………$~C………………^M~Ctankerületi igazgató" #\tab #\tab #\tab)
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "Tk ig")))

    (,(format nil "$………………$^M~Cgazdasági vezetõ^M" #\tab)
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (xcref (tk-row a) "Gazdasági vez.")))
    
;    (,(format nil "~C$………………$^M~Ctankerületi igazgató~Cmunkavállaló^M" #\tab #\tab #\tab)
;    (,(format nil "~C$………………$^M~Cszakmai vezetõ~Cmunkavállaló^M" #\tab #\tab #\tab) ; SIÓFOK
    (,(format nil "~C$…..….…..….$^M" #\tab) ; ÉRD, és mostantól legyen így!
     ,(vals-fn ((a "Név"))
        (clean-name a)))

    ("számára^M^M^M$………………$ Tankerületi Központnál "
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (astring-capitalize 
         (add-article
          (first (str:words a))))))

    (" Tankerületi Központnál $………………$ napjától fennálló köznevelési "
     ,(vals-fn ((a "Belépés dátuma"))
        (excel-date-string a :words t)))

    ("az Ön kinevezését $………………$ napi hatállyal az alábbiak"
     ,(vals-fn ()
        (declare (ignore xarray))
        *mod-start*))

    ("számára^M^M^M$………………$ Tankerületi Központ ("
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (astring-capitalize 
         (add-article
          (first (str:words a))))))

    ("A felek megállapodnak abban, hogy a közöttük $………………$ napjától f"
     ,(vals-fn ((a "Belépés dátuma"))
        (excel-date-string a :words t)))

    ("közös megegyezéssel $………………$ napi hatállyal "
     ,(vals-fn ()
        *mod-start*))
    
    ("Központnál mint munkáltatónál $………………$ napjától fennálló"
     ,(vals-fn ((a "Belépés dátuma"))
        (excel-date-string a :words t)))

    ("A munkáltatónál $………………$ napjától fennálló "
     ,(vals-fn ((a "Belépés dátuma"))
        (excel-date-string a :words t)))

    ("elõresorolom Önt $………………$ fokozatba."
     ,(vals-fn ((a "Bérrendsz. csop név"))
        (str:unwords (str:words
         (str:trim a)))))




    
    ("Az Mt. 136. § (1) bekezdése és 153. § (1) bekezdése$, valamint a pedagógusok új életpályájáról szóló 2023. évi LII. törvény (a továbbiakban: Púétv.) végrehajtásáról szóló 401/2023. (VIII. 30.) Korm. rendelet 95. § (1) bekezdése$ alapján a munkavállaló "
     ,(vals-fn ((ho "Heti óra")) :fees fees
        (let* ((sum  (getf (first fees) :sum))
               (prop (* 100 (round (* (/ sum ho) 40) 100))))
          (if (< prop 348800)
            ""
            ", valamint a pedagógusok új életpályájáról szóló 2023. évi LII. törvény (a továbbiakban: Púétv.) végrehajtásáról szóló 401/2023. (VIII. 30.) Korm. rendelet 95. § (1) bekezdése"))))


    ("A munkavállaló a munkaviszonyból származó igényének érvényesítése érdekében a $Púétv.$ 132. § (1) bekezdése"
     ,(vals-fn ((ho "Heti óra")) :fees fees
        (let* ((sum  (getf (first fees) :sum))
               (prop (* 100 (round (* (/ sum ho) 40) 100))))
          (if (< prop 348800)
            "pedagógusok új életpályájáról szóló 2023. évi LII. törvény"
            "Púétv."))))

    ))


(defun temp-target (temp)
  (let* ((external (carriage-return temp))
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
                           (?content current)))
                 (found  (range-find-text range clean)))
          (when found
            (cclet* ((start (+ found start-offset))
                     (end   (+ found end-offset))
                     (text  (format nil "~a" (funcall val-fn xarray))))
              (selection-overwrite range start end text))))))))


(defun copy-via-fragment (from to)
  (let ((fragment (tempfile)))
    (!exportfragment (?formattedtext from)
                      fragment
                      +wd-format-document-default+)
    (!importfragment to fragment)
    (delete-file fragment)))


(defconstant +wd-field-page+ 33)
(defconstant +wd-header-footer-even-pages+ 3)
(defconstant +wd-align-paragraph-center+ 1)


;;; Newer version using fragments.
(defun add-template (word doc xarray first-doc page-break-needed)
  ;; Új temp file dok.sablon alapján
  (let ((temp-name (doctemplate xarray)))
    (when temp-name
      (with-document (:doc current :app word :open temp-name :read-only nil :close t :save nil) ; Mohács?
        ;; Adatok beillesztése táblázatból
        (fill-template current xarray)
        ;; Oldaltörés beillesztése
        (when (and (not first-doc) page-break-needed)
          (!insertbreak (end-of-doc doc) +wd-page-break+)
          (!insertbreak (end-of-doc doc) +wd-section-break-odd-page+))
        ;; Jelen SZTSZ dok. másolása
        (cclet* ((sect-src (?range (?first (?sections current))))
                 (sect-trg (?range (?last  (?sections doc)))))
          (!wholestory sect-src)
          (copy-via-fragment (?formattedtext sect-src) sect-trg))
        (cclet* ((sect-trg (?last (?sections doc)))
                 (pri-head (!item (?headers sect-trg) +wd-header-footer-primary+))
                 (pg-nums  (?pagenumbers pri-head))
                 (pg-setup (?pagesetup sect-trg)))
          (setf (?text (?range pri-head)) "")                ; Meglévõ elsõdleges fejléc szövegének törlése
          (!add pg-nums +wd-align-page-number-center+ nil)   ; Oldalszámozás középre
          (setf (?restartnumberingatsection pg-nums) t       ; Oldalszámozás újrakezdése szakaszonként
                (?startingnumber pg-nums) 1                  ; Oldalszámozás kezdése 1-tõl (elsõ o. beleszámítva)
                (?differentfirstpageheaderfooter pg-setup) t ; Elsõ oldalon eltérõ fejléc/lábléc
                (?mirrormargins pg-setup) t                  ; Tükörmargók
                (?oddandevenpagesheaderfooter pg-setup) t)   ; Eltérõ páros- és páratlan oldalak
          (cclet* ((evenh (!item (?headers sect-trg) +wd-header-footer-even-pages+))
                   (headr (?range evenh)))
;            (!add (?fields headr) headr +wd-field-page+ :empty nil)
            (setf (?alignment (?paragraphformat headr)) +wd-align-paragraph-center+
                  (?name (?font headr)) "Times New Roman"
                  (?size (?font headr)) 12))))
      ;; Eredmény állapotának mentése
      (!save doc)
      t)))


;;; Elválasztó rajzolása (progress ablakhoz)
(defun line (n &optional (char #\-))
  (format nil "~v@{~A~:*~}" n char))


(defconstant +wd-format-pdf+ 17)


;;; Személyi kör feldolgozása, minden SZTSZ külön fájlba.
(defun process-ungrouped-ps (tk-ps-only ps dump step-progress-indicator quit-on-abort word)
  ;; Iteráció SZTSZ-eken:
  (xadouniques (sztsz tk-ps-only "SZTSZ")
;    (let* ((sztsz-only (xaselect tk-ps-only #'(lambda (row) (astring= (xcref row "SZTSZ") sztsz))))
    (let* ((sztszp (round (parse-number sztsz)))
           (sztsz-only (xaselect tk-ps-only #'(lambda (row) (= (parse-number (xcref row "SZTSZ"))
                                                               sztszp))))
;                                                               (parse-number sztsz)))))
#|    (let* ((sztsz-only (xaselect tk-ps-only #'(lambda (row) (string= (parse-number-as-string (xcref row "SZTSZ") 8)
                                                                     (parse-number-as-string sztsz 8)))))|#
           (filename   (newfile-ungrouped (xarows sztsz-only 0))))
      ;; Ha személyi körhöz nincs definiálva doctype:
      (if (not filename)
        (progn 
          (funcall dump "SZTSZ: ~a   kihagyva, a ~a személyi körhöz nincs definiálva dokumentumsablon.~%" sztszp ps)
          (funcall step-progress-indicator))
        ;; Ha van:
        (progn
          (with-document (:doc output :app word :close t :save t)
            (!saveas2 output filename)
            (funcall dump "SZTSZ: ~a" sztszp)
            ;; SZTSZ adatainak beírása a dokumentumba.
            (if (add-template word output sztsz-only t nil)
              (progn
                (!saveas2 output (namestring (make-pathname :type "pdf" :defaults filename))
                         +wd-format-pdf+)
                (funcall dump "  ok~%"))
              (funcall dump "  HIBA!~%")))
          (funcall step-progress-indicator)
          (funcall quit-on-abort))))))


;;; Dokumentumok generálása
(defun process ()
  (with-wax-errorsink
   (with-property-accessors
     (setf (errorsink-on) nil;t
           (property-accessors-on) t)
     (cclet* ((tk-head "Vállalat hosszú megnevezése")
              (word    (com:create-object :progid "Word.Application"))
              (query   nil))
       (setf (?visible word) nil)
       ;; Lekérdezés táblázat tartalmának betöltése
       (with-workbook (:open *xls-query* :read-only t :wsvars (ws-query) :close t)
         (setf query (read-xarray (used-range ws-query))))
       ;; Progress bar
       (with-progress ("Dokumentumok generálása" quit-on-abort dump step-progress-indicator
;                       (length (xauniques query "SZTSZ" :test #'astring=)))
                       (length (xauniques query "SZTSZ")))
         (dump "~%~%")
         ;; Iteráció TK-kon.
         (xadouniques  (tk query tk-head)
           (dump "~%~a~%~a~%~a~%~%" (line 70 #\=) (astring-upcase tk) (line 70 #\=))
           ;; Iteráció személyi körökön.
           (let ((tk-only (xaselect query #'(lambda (row) (astring= (xcref row tk-head) tk)))))
             (xadouniques (ps tk-only "SZK")
               (dump "~a személyi kör  ~a~%" ps (line (- 70 (+ (length ps) 15))))
               ;; Személyi kör sorok.
               (let ((tk-ps-only (xaselect tk-only #'(lambda (row) (astring= (xcref row "SZK") ps)))))
                 (process-ungrouped-ps tk-ps-only ps #'dump #'step-progress-indicator #'quit-on-abort word))))))
       (!quit word)))))


;;; ----------------------------------------------------------------------
;;; Main


;;; Globális változók értékének mentése köv. munkamenethez.
(defun save-state ()
  (let ((filename (appfile "state.txt")))
    (when (probe-file filename)
      (hide-file filename nil))
    (save-forms
     filename
     `(:doctype  ,*doctype*
       :query1   ,*xls-query*
       :query2   ,*xls-query2*
       :kir      ,*kir-wbook*
       :tempdir  ,*doc-template-dir*
       :outdir   ,*out-dir*
       :modstart ,*mod-start*
       :filenos  ,*fileno-wbook*))
;    (uiop:run-program (list "attrib" "+h" (namestring filename)))))
    (hide-file filename t)))


;;; Elõzõ munkamenet mentett adatainak visszatöltése a globális változókba.
(defun load-state ()
  (let ((filename (appfile "state.txt")))
    (hide-file filename nil)
    (let ((state (load-forms (appfile "state.txt"))))
      (when state
        (destructuring-bind (&key doctype query1 query2 kir tempdir outdir modstart grouped filenos
                                  &allow-other-keys)
            state
          (setf *doctype*          doctype
                *xls-query*        query1
                *xls-query2*       query2
                *kir-wbook*        kir
                *doc-template-dir* tempdir
                *out-dir*          outdir
                *xls-tks*          (namestring (merge-pathnames *xls-tks-filename* tempdir))
                *mod-start*        modstart
                *fileno-wbook*     filenos
                ))))
    (hide-file filename t)))


;;; Vezérlõ globális változók alaphelyzetbe állítása.
(defun init-state ()
  (setf *doctype*          (getf (first *doctypes*) :name)
        *xls-query*        (appdir)
        *xls-query2*       ""
        *kir-wbook*        ""
        *doc-template-dir* (appdir)
        *out-dir*          (appdir)
        *xls-tks*          (namestring (merge-pathnames *xls-tks-filename* (appdir)))
        *mod-start*        *mod-start-def*
        *fileno-wbook*     ""
        ))


;;; Dokumentumsablon-almappák ellenõrzése.
(defun temp-subdirs-found-p ()
  (let* ((subdirs (mapcar #'(lambda (rec)
                              (getf rec :dir))
                          *doctypes*))
         (subdirs-found
          (mapcar #'(lambda (subdir)
                      (probe-file
                       (concatenate 'string *doc-template-dir* subdir)))
                  subdirs)))
    (not (member nil subdirs-found))))


(defparameter *runningp* nil) ; A "Dokumentumok generálása" gomba csak akkor indítja el a folyamatot, ha ez NIL.
(defparameter *filereq-filter-xlsx* '("Excel fájlok" "*.xlsx" "Minden fájl" "*.*"))


;;; main();
(defun start ()
  ;; Vezérlõ glob. változók alapállapotba
  (init-state)
  ;; Ha van mentett "state.txt", a benne lévõ adatokat ráírjuk a glob. változókra
  (load-state)
  ;; Fõablak létrehozása
  (wg-window
   "Iratgeneráló (Egyoldalú kinevezésmódosítás, köznevelési foglalkoztatottak, 2025.jan)"
   (wg-options " Dokumentumtípus választása                "
               #'(lambda (text &rest rest)
                   (declare (ignore rest))
                   (setf *doctype* text))
               (mapcar #'(lambda (rec)
                           (getf rec :name))
                       *doctypes*)
               *doctype*)
   (wg-text-input " Tanév/módosítás érvényesség kezdete"
                  #'(lambda (text &rest rest)
                      (declare (ignore rest))
                      (setf *mod-start* text))
                  *mod-start*)
   (wg-file-selector " SAP lekérdezés eredménye                   "
                     "SAP lekérdezés eredménye"
                     (second *filereq-filter-xlsx*)
                     *filereq-filter-xlsx*
                     #'(lambda (text &rest rest)
                         (declare (ignore rest))
                         (setf *xls-query* text))
                     *xls-query*)
   (wg-file-selector " Iktatószámok listája (opcionális)             "
                     "Iktatószámok listája"
                     (second *filereq-filter-xlsx*)
                     *filereq-filter-xlsx*
                     #'(lambda (text &rest rest)
                         (declare (ignore rest))
                         (setf *fileno-wbook* text)
                         (when (string= text "")
                           (setf *fileno-data* nil)))
                     *fileno-wbook*
                     :cancel #'(lambda () (setf *fileno-wbook* ""
                                                *fileno-data* nil)))
   (wg-file-selector " KIR feladatellátási helyek (opcionális)    "
                     "KIR feladatellátási helyek listája"
                     (second *filereq-filter-xlsx*)
                     *filereq-filter-xlsx*
                     #'(lambda (text &rest rest)
                         (declare (ignore rest))
                         (setf *kir-wbook* text)
                         (when (string= text "")
                           (setf *kir-data* nil)))
                     *kir-wbook*
                     :cancel #'(lambda () (setf *kir-wbook* ""
                                                *kir-data* nil)))
   (wg-dir-selector " Dokumentumsablonok mappája             "
                    "Dokumentumsablonok mappája"
                    #'(lambda (text &rest rest)
                        (declare (ignore rest))
                        (setf *doc-template-dir* text
                              *xls-tks* (namestring (merge-pathnames *xls-tks-filename* text ))))
                    *doc-template-dir*)
   (wg-dir-selector " Generált dokumentumok mappája         "
                    "Generált dokumentumok mappája"
                    #'(lambda (text &rest rest)
                        (declare (ignore rest))
                        (setf *out-dir* text))
                    *out-dir*)
#|   (wg-text-input " Iktatószám-sablon                                  "
                  #'(lambda (text &rest rest)
                      (declare (ignore rest))
                      (setf *file-number-temp* text))
                  *file-number-temp*)|#
   (wg-button " Dokumentumok generálása"
              #'(lambda (interface)
                  (declare (ignore interface))
                  ;; Ha dok.sablon almappák megvannak, indítás, egyébként figyelmeztetés.
                  (if (not (temp-subdirs-found-p))
                    (wg-msg "A dokumentumsablonok kiválasztott mappája érvénytelen!~%Kérem szíveskedjen azt a mappát kiválasztani, amelyik az \"Egyoldalú kinevezésmódosítások\", \"Kétoldalú kinevezésmódosítások\" és \"Kinevezések\" almappákat tartalmazza.")
                    (unless *runningp*
                      ;; Ha még nem fut, indítás.
                      (let ((*runningp* t))
                        (wg-floating-message "Indítás ...")
                        (save-state)
                        (process))))))))



;;; ----------------------------------------------------------------------
;;; Sandbox



(defun x ()
  (with-property-accessors t
;    (saving-with-intermediate-temp ("c:\\Users\\cselovszkid\\abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghij.xlsx" fn)
    (saving-with-intermediate-temp ("c:\\Users\\cselovszkid\\abcd.xlsx" fn)
      (with-workbook (:wbook wbook :wsvars (wsheet) :save t)
        (setf (xcell wsheet 1 1) "Áhhááááá!!!")
        (!saveas wbook (fn))))))


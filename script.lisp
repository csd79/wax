;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)
#.(enable-ccom-syntax)


;;; ----------------------------------------------------------------------
;;; Globális változók


(defparameter *xls-tks-filename*  "TK vezetõk.xlsx")
(defparameter *mod-start-default* "2026. január 1.") ;;; Ez a jelen dátum függvényében jeles dátumokra ugorhatna... jan1 sep1


;;; ----------------------------------------------------------------------
;;; Generálható dokumentumtípusok


(defun szk-fn (szk)
  #'(lambda (xarray)
      (string= (xcref xarray "SZK") szk)))

(defun b1-noks-fn ()
  #'(lambda (xarray)
      (and (string= (xcref xarray "SZK") "B1")
           (member (xcref xarray :x) '("4336" "4211" "4214") :test #'string=))))

(defun b1-kiseg-fn ()
  #'(lambda (xarray)
      (and (string= (xcref xarray "SZK") "B1")
           (not (member (xcref xarray :x) '("4336" "4211" "4214") :test #'string=)))))

(defparameter *doctypes*
  `((:name "Kinevezések"
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


(defun select-doctype (xarray doctype)
  (when (string/= doctype "")
    (let* ((type (find doctype *doctypes* :test #'string= :key #'(lambda (rec)
                                                                     (getf rec :name))))
           (szk  (find-if #'identity (getf type :szk) :key #'(lambda (rec)
                                                               (funcall (first rec) xarray)))))
      (when szk
        (append (list (getf type :dir))
                (cdr szk))))))


(defun remove-wrapping (chars string)
  (flet ((in-there-p (current) (member current chars)))
    (let ((start (position-if-not #'in-there-p string))
          (end   (position-if-not #'in-there-p string :from-end t)))
      (subseq string start (1+ end)))))


(defun concpath (&rest strings)
  (when strings
    (let ((words (loop for w in strings collecting
                       (str:ensure-prefix
                        "\\" (remove-wrapping '(#\\) w)))))
      (subseq (apply #'concatenate 'string words) 1))))


(defun doctemplate (xarray obj)
  (destructuring-bind (&optional subdir szk template)
      (select-doctype xarray (get-state obj :doctype))
    (declare (ignore szk))
    (when template
      (concpath (get-state obj :doctemp-dir)
                subdir template))))


(defun newfile (xarray obj)
  (destructuring-bind (&optional subdir szk template)
      (select-doctype xarray (get-state obj :doctype))
    (declare (ignore subdir))
    (when template
      (let* ((tk      (xcref xarray "Vállalat hosszú megnevezése"))
             (tk-ok   (remove-illegal-filename-chars (format nil "~a TK" (first (str:words tk)))))
             (name-ok (remove-illegal-filename-chars (clean-name (xcref xarray "Név")))))
        (format nil "~a~a, ~a, ~a, ~a, ~a" (get-state obj :results-dir) tk-ok szk name-ok
                (timestamp (get-universal-time))
                template)))))


(defun tempfile (obj type)
  (format nil "~a~a_~a~a"
          (get-state obj :results-dir) "temp" (timestamp (get-universal-time)) type))


;;; ----------------------------------------------------------------------
;;; Törzs


(defun get-fee-row (row cols codes)
  (apply #'append
         (mapcar #'(lambda (col code)
                     (list code (xcref row col)))
                 cols codes)))


(defun get-fees (xarray)
  (let ((result '())
        (codes  '(:code :name :sum :measure :end :titl :cstart)))
    (do-xarows (row r xarray)
      (push (get-fee-row row '(15 16 17 36 30 39 43) codes) result)
      (push (get-fee-row row '(19 20 21 35 31 39 35) codes) result))
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
                                   (mtitl (getf meta :titl)))
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


(defun correct-1125-p (fee obj)
  "Is starting date = 09.01. of the year indicated by MOD-START?"
  (let* ((mod-start (parse-hudate (get-state obj :mod-start)))
         (correct-start (if (>= (second mod-start) 9)
                          (list (first mod-start) 9 1)
                          (list (1- (first mod-start)) 9 1))))
    ;; Ha Kinevezés, a kezdõdátumtól függ,
    (if (string= (get-state obj :doctype) "Kinevezések")
      (equal (excel-date (getf fee :cstart)) correct-start)
      ;; ha nem, mindig korrekt.
      t)))


(defun remove-incorrect-1125 (fees obj)
  "Remove fee code 1125 when it's starting date is invalid."
  (remove-if #'(lambda (fee)
                 (and (string= (getf fee :code) "1125")
                      (not (correct-1125-p fee obj))))
             fees))


(defmessenger vals-error ((error) field row cols fees obj)
;  "~a:~%   SZEMÉLY: ~a, ~a~%   TK: ~a~%   INTÉZMÉNY: ~a~%   SZEMÉLYI KÖR: ~a~%   ÉRINTETT OSZLOP(OK):~%~{~{      \"~a\" = \"~a\"~}~%~}~a   HIBAÜZENET: ~a~%~a~3%"
  "~a:~%   SZEMÉLY: ~a, ~a~%   TK: ~a~%   INTÉZMÉNY: ~a~%   SZEMÉLYI KÖR: ~a~%   ÉRINTETT OSZLOP(OK):~%~{~{      \"~a\" = \"~a\"~}~%~}~a   HIBAÜZENET: ~a~3%"
  (if (string/= field "")
    (format nil "Hiba a \"~a\" mezõ kitöltése közben" field)
    "Hiba egy mezõ kitöltése közben")                           ; field
  (str:capitalize (xcref row :c))                               ; name
  (xcref row :b)                                                ; sztsz
  (str:replace-first "Tankerületi Központ" "TK" (xcref row :a)) ; TK
  (xcref row :z)                                                ; institute
  (xcref row :ag)                                               ; group
  (mapcar #'(lambda (col) (list col (xcref row col))) cols)
  (if (or fees obj)
    (format nil "   EGYÉB LEHETSÉGES OK: ~a~a~a~%"
            (if fees "bérelemek" "")
            (if (and fees obj) ", " "")
            (if obj "iktatószám táblázat, TK adatok, KIR adatok, GUI dátum, jogviszony beszámítás"))
    "")
  error
;  (backtrace->string 2)
  )


(defmacro vals-fn (binds &body body)
  (let ((clauses   '())
        (slim-body (remove nil ; Why is this needed?
                           (remove-pairs body (list :fees :obj :field))))
        (fees      (ignore-errors (getf body :fees))) ; IGNORE-ERRORS shouldn't be needed here, GETF
        (obj       (ignore-errors (getf body :obj)))  ;   return NIL when value is not in plist
        (field     (ignore-errors (getf body :field))))
    ;; Create a list of LET-clauses for every column in BINDS
    (dolist (pair binds)
      (destructuring-bind (&optional symbol column)
          pair
        (when (and symbol column)
          (push (list symbol `(xcref row ,column)) clauses))))
    ;; When prescribed, add FEES to CLAUSES
    (when fees (push (list fees '(get-fees row)) clauses))
    ;; When prescribed, add OBJ to CLAUSES
    (when obj  (push (list obj 'obj) clauses))
    `(lambda (row obj)
       (skippable (condition
                   'doc-exit
                   (vals-error ,(or field "") row ',(mapcar #'second binds) ,(when fees t) ,(when obj t)))
         (let ,clauses
           ,@slim-body)))))


;;; ----------------------------------------------------------------------
;;; Átírások



(defun tks-row (obj tk column)
  (let ((row (select-row-from obj :tks #'(lambda (row) (string= (xcref row "TK") tk)))))
    (when row
      (xcref row column))))


(defun school-year-end (list)
  (destructuring-bind (year month &optional day)
      list
    (declare (ignore day))
    (let ((result
           (list (if (>= month 9) (1+ year) year)
                 8 31)))
      result)))


(defparameter *t2*
  `(
    ("Iktatószám: $………………$^M"
     ,(vals-fn ((sztsz "SZTSZ")) :obj obj
        (let ((row (select-row-from obj :filenum #'(lambda (row) (= (parse-number sztsz)
                                                                    (parse-number (xcref row "SZTSZ")))))))
          (if (and row
                   (not (zerop (xarray-indexed-height row))))
            (xcref row "Iktatószám")
            "………………"))))

    ("$………………$^MTANKERÜLETI^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (let ((tk (tks-row obj a "TK")))
          (when tk
            (astring-upcase (first (str:words tk))))))
     ,#'(lambda (doc)
          (header doc 1 +wd-header-footer-first-page+)))

    ("Székhelye: $………………$^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Székhely"))
     ,#'(lambda (doc)
          (footer doc 1 +wd-header-footer-first-page+)))

    ("Törzskönyvi azonosító szám: $………………$^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (let ((tsz (tks-row obj a "Törzsszám")))
          (when tsz (round tsz))))
     ,#'(lambda (doc)
          (footer doc 1 +wd-header-footer-first-page+)))
    
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
     ,(vals-fn ((a "Belépés dátuma")) :field "kinevezés/módosítás kezdete"
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

    ("$^MA …-jogszabály-… alapján próbaidõ nem köthetõ ki.^M^M$"
     ,(vals-fn ((szk "SZK") (bd "Belépés dátuma") (pv "Próbaidõ  vége"))
        (let* ((pv-str (if (empty-cell-p pv)
                         "………"
                         (excel-date-string pv :words t)))
               (period (concatenate 'string (excel-date-string bd :words t) " napjától " pv-str))
               (b1-prob (concatenate 'string "^MA munka törvénykönyvérõl szóló 2012. évi I. törvény (a továbbiakban: Mt.) 45. § (5) bekezdése alapján a felek " period " napjáig terjedõ próbaidõt kötnek ki, amely idõtartam alatt a munkaviszonyt az Mt. 79. § (1) bekezdésének a) pontja alapján bármelyik fél azonnali hatályú felmondással – indokolás nélkül – megszüntetheti.^M^M"))
               (bx-prob (concatenate 'string "^MA Púétv. 41. § (1) bekezdése alapján " period " napjáig tartó próbaidõt kötök ki, amely idõtartam alatt a köznevelési foglalkoztatotti jogviszonyt a Púétv. 41. § (4) bekezdése és 46. § (2) bekezdésének a) pontja alapján bármelyik fél indokolás nélkül azonnali hatállyal megszüntetheti.^M^M")))
          (if (string= szk "B1")
            (if (empty-cell-p pv)
              ""
              b1-prob)
            ;; Egyéb személyi körök
            (if (empty-cell-p pv)
              "^MA …-jogszabály-… alapján próbaidõ nem köthetõ ki.^M^M"
              bx-prob)))))

    (,(format nil "unkaköre:~C$………………$^M" #\tab)
     ,(vals-fn ((a :y))
        (str:unwords (str:words
         (str:trim a)))))

    (,(format nil "Munkavégzésének helye:~C$……………………………………………………$" #\tab)
     ,(vals-fn ((a "szervezeti egys hosszú megnev.")
                (b "Szervezeti egység OM azonosító") (c "Szerv.egység feladat ellát hel")) :obj obj
        (let ((kir-file (get-state obj :kir-file)))
          (if (and (not (empty-cell-p b))
                   (not (empty-cell-p c))
                   (string/= kir-file ""))
            ;; KIR táblából véve
            (let ((kir-row (select-row-from
                            obj :kir
                            #'(lambda (row)
                                (and 
                                 (= (parse-number b) (parse-number (xcref row "OM azonosító")))
                                 (= (parse-number c) (parse-number
                                                      (xcref row "A feladatellátási hely sorszáma"))))))))
              (if (and kir-row (not (xarray-zero-index-p kir-row)))
                (xcref kir-row "A feladatellátási hely megnevezése")
                (str:unwords (str:words (str:trim a)))))
            ;; SAP-ból véve
            (str:unwords (str:words (str:trim a)))))))

    (", $cím$^Mvagy^MMunkavégzés"
     ,(vals-fn ((a "Szervezeti egység OM azonosító") (b "Szerv.egység feladat ellát hel")) :obj obj
        (let ((kir-file (get-state obj :kir-file)))
          (if (and (not (empty-cell-p a))
                   (not (empty-cell-p b))
                   (string/= kir-file ""))
            ;; Feladatellátási hely címe
            (let ((kir-row (select-row-from
                            obj :kir
                            #'(lambda (row)
                                (and 
                                 (= (parse-number a) (parse-number (xcref row "OM azonosító")))
                                 (= (parse-number b) (parse-number
                                                       (xcref row "A feladatellátási hely sorszáma"))))))))
              (if (and kir-row
                       (not (xarray-zero-index-p kir-row)))
                (format nil "~D ~a, ~a"
                        (round (xcref kir-row "A feladatellátási hely irányítószáma"))
                        (xcref kir-row "A feladatellátási hely települése")
                        (xcref kir-row "A feladatellátási hely pontos címe"))
                "cím"))
            ;; Nincs cím
            "cím"))))
    
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
     ,(vals-fn () :obj obj
        (declare (ignore xarray))
        (get-state obj :mod-start)))

    ("Havi illetményét $………………$ napi hatállyal" ; Kétoldalú
     ,(vals-fn () :obj obj
        (declare (ignore xarray))
        (get-state obj :mod-start)))

    (" heti munkaidejére tekintettel – $………………$ alapján az alábbiak szerint állapítom meg.^M"
     ,(vals-fn ((szk "SZK") (bes "Bérrendsz. csop név") (eila "Esélyteremtési illetményrészre")
                (titl "CÍm")) :fees fees :obj obj :field "jogszabályi hivatkozás"
        (let ((fees    (remove-incorrect-1125 fees obj))
              (cref::*coderefs*  cref::*puetv-b1b2b8b9-illetmenyelemek-current*)
              (cref::*codenames* cref::*puetv-megnevezes-current*)
              (cref::*defined-tvs* (if (string= bes "Gyakornok")
                                     '("1puetv" "2puetv-vhr")
                                     '("1puetv"))))
          (let* ((codes (mapcar #'(lambda (fee) (getf fee :code)) fees))
                 (fees  (cref::fees :codes codes :ps szk :lab bes :eila eila :titl titl))
                 (text  (cref::convert fees)))
            text))))

    ("napi hatállyal – besorolására$………………$ és"
     ,(vals-fn ((szk "SZK") (bes "Bérrendsz. csop név") (eila "Esélyteremtési illetményrészre")
                (titl "CÍm")) :fees fees :obj obj
        (let ((fees (remove-incorrect-1125 fees obj))
              (cref::*coderefs*  cref::*puetv-b1b2b8b9-illetmenyelemek-current*)
              (cref::*codenames* cref::*puetv-megnevezes-current*)
              (cref::*defined-tvs* (if (string= bes "Gyakornok")
                                     '("1puetv" "2puetv-vhr")
                                     '("1puetv"))))
          (let* ((codes (mapcar #'(lambda (fee) (getf fee :code)) fees))
                 (fees  (cref::fees :codes codes :ps szk :lab bes :eila eila :titl titl)))
            (if (or (member :ter-illemeles-ped fees)
                    (member :ter-illemeles-pednoks fees))
              ", a 2024/2025. tanítási évre vonatkozó teljesítményértékelésének eredményére"
              "")))))
   
    (,(format nil "$Havi illetmény:~C………………~CFt^MIlletmény összesen:~C………………~cFt$^M" #\tab #\tab #\tab #\tab)
     ,(vals-fn ((bd "Belépés dátuma") (hiv "Szerz.vége") (eila "Esélyteremtési illetményrészre"))
        :fees fees :obj obj
        (let* ((fees    (remove-incorrect-1125 fees obj))
               (ordered (sort-fees fees cref::*puetv-b1b2b8b9-illetmenyelemek-current-sorrend*))
               (total   0)
               (digest  (mapcar #'(lambda (fee)
                                    (destructuring-bind (&key code name sum measure end titl cstart) fee
                                      (declare (ignore name))
                                      (incf total sum)
                                      (append
                                       ;; Ill.e. megnevezés
                                          ;; CREF FORRÁS OBJ-BAN????
                                       (list (fee-name code eila titl cref::*puetv-b1b2b8b9-illetmenyelemek-current*)
                                             ;; Összeg
                                             (currency sum))
                                       ;; Megállapítás idõszak kezdete:
                                       ;;   Havi ill., mesterfok vagy TÉR: nem kell feltüntetni.
                                       (cond ((member code '("1P00" "1116" "1125") :test #'string=)
                                              nil)
                                             ;; Esélyteremtési v. egyes tantárgyak után járó:
                                             ;;   belépés dátuma vagy tanévkezdet (amelyik késõbbi)
                                             ((member code '("1114" "1115") :test #'string=)
                                              (list
                                               (if (> (hudate->unitime (excel-date bd))
                                                      (hudate->unitime (parse-hudate (get-state obj :mod-start))))
                                                 (excel-date-string bd :words t)
                                                 (get-state obj :mod-start))))
                                             ;; Egyébként: ill.érvényesség kezdete; ha nincs: belépés dátuma.
                                            (t (list (excel-date-string (or cstart bd) :words t))))
                                       ;; Vége dátum:
                                       ;;   Esélyteremtési v. egyes tantárgyak után járó:
                                       ;;     ha határozott idõ vége meg van adva és kisebb mint tanév vége:
                                       ;;       hat.idõ vége
                                       ;;     különben:
                                       ;;       tanév vége
                                       (cond ((member code '("1114" "1115") :test #'string=)
                                              (let ((end (apply #'ccoffice:date-to-excel-serial 
                                                                (school-year-end (parse-hudate
                                                                                  (get-state obj :mod-start))))))
                                                (list (excel-date-string
                                                       (if (empty-cell-p hiv)
                                                         end
                                                         (min end hiv))
                                                       :words t))))
                                             ;; Egyébként, ha nem havi illetmény, és az ill.érv.vége
                                             ;;   meg van adva és nem 9999.12.31:
                                             ;;     ill.érv. vége
                                             ((and
                                               (string/= code "1P00")
                                               (not (empty-cell-p end))
                                               (/= end 2958465))
                                              (list (excel-date-string end :words t)))
                                             ;; Egyébként: nem kell feltüntetni.
                                             ;; TÖMEGES RÖGZÍTÉSNÉL GYAKRAN HATÁROZOTT IDÕ VÉGE UTÁN DÁTUM
                                             ;; KERÜLT RÖGZÍTÉSRE ÉRVÉNYESSÉG VÉGEKÉNT, EZÉRT NEM JELENIK MEG!!!
                                             (t nil)))))
                                ordered))
               (lines  '()))
          (dolist (cookin digest)
            (destructuring-bind (name sum &optional measure end) cookin
              (push (format nil "~a:~C~a~CFt~C" name #\tab sum #\tab #\return) lines)
              (when measure
                (if end
                  (push (format nil "megállapításának idõszaka: ~a napjától ~a napjáig~C" measure end #\return) lines)
                  (push (format nil "megállapításának idõszaka: ~a napjától~C" measure #\return) lines)))))
          (push (format nil "Illetmény összesen:~C~a~CFt" #\tab (currency total) #\tab) lines)
          (apply #'concatenate 'string
                 (nreverse lines)))))

    ("illetékes törvényszékhez.^M^M$………………$,"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Helységnév")))

    ("Pénzügyileg ellenjegyzem.^M^M$………………$,"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Helységnév")))
        
    (,(format nil "^M~C$NÉV$~C" #\tab #\tab)
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "TK ig")))

    ("$NÉV$^Mtankerületi igazgató^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "TK ig")))

    (,(format nil "~C$NÉV$^M~Cgazdasági vezetõ^M" #\tab #\tab)
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Gazdasági vez.")))

    ("^M$NÉV$^Mgazdasági vezetõ^M"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Gazdasági vez.")))

    (,(format nil "~C$NÉV$^M~Cköznevelési foglalkoztatotti" #\tab #\tab)
     ,(vals-fn ((a "Név"))
        (clean-name a)))

    ("egyrészrõl $………………$ Tankerületi Központ"
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (add-article
         (first (str:words a)))))

    ("(székhelye: $………………$, t"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Székhely")))

    ("nyvi azonosító szám: $………………$, ké"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (let ((tsz (tks-row obj a "Törzsszám")))
          (when tsz (round tsz)))))

    (", képviseli: $………………$ tankerületi igazgató)"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "TK ig")))

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
         ;; Határozatlan ideju kinevezés/szerzõdés
         ((member a '("Határozatlan id.kine" "Hatlan. ideju MT sz.") :test #'string=)
          "határozatlan idejû")
         ;; Határozott ideju helyettesítõ
         ((notany #'empty-cell-p (list c d))
          (format nil "~a tartósan távollévõ helyettesítése céljából határozott ideig, várhatóan ~a napjáig tartó"
                  ;; Helyettesített dolgozó
                  (if (string/= c "")
                    (astring-capitalize c)
                    "………………")
                  ;; Szerzõdés vége
                  (excel-date-string d :words t)))
         ;; Határozott ideju nem-helyettesítõ
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
        (destructuring-bind (&key code name sum measure end titl cstart)
            (first fees)
          (declare (ignore code name measure end cstart))
          (format nil "~a Ft, azaz ~a"
                  (currency sum)
                  (sub->words sum)))))

    (,(format nil "^M~C$………………$, 2025" #\tab)
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Helységnév")))

    (,(format nil "Pénzügyileg ellenjegyzem.^M^M~C$………………$" #\tab)
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Helységnév")))

    (,(format nil "~C$………………$~C………………^M~Ctankerületi igazgató" #\tab #\tab #\tab)
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Tk ig")))

    (,(format nil "$………………$^M~Cgazdasági vezetõ^M" #\tab)
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Gazdasági vez.")))
    
    (,(format nil "~C$NÉV$^M~Ctankerületi igazgató~Cmunkavállaló^M" #\tab #\tab #\tab)
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
     ,(vals-fn () :obj obj
        (declare (ignore xarray))
        (get-state obj :mod-start)))

    ("számára^M^M^M$………………$ Tankerületi Központ ("
     ,(vals-fn ((a "Vállalat hosszú megnevezése"))
        (astring-capitalize 
         (add-article
          (first (str:words a))))))

    ("A felek megállapodnak abban, hogy a közöttük $………………$ napjától f"
     ,(vals-fn ((a "Belépés dátuma"))
        (excel-date-string a :words t)))

    ("közös megegyezéssel $………………$ napi hatállyal "
     ,(vals-fn () :obj obj
        (get-state obj :mod-start)))
    
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

    ("$<><><>$"
     ,(vals-fn ((sztsz "SZTSZ") (date-cl "Jv.kezd.fiz.fokozathoz") (date-bonus  "Jv.kezd.jubileumhoz")
                (date-sever "Jv.kezd.végkielégítéshez")) :obj obj
        (let ((found (select-row-from obj :prevrels #'(lambda (row)
                                                        (let ((s (xcref row "Szem.ügyi törzsszám")))
                                                          (and (not (empty-cell-p s))
                                                               (= (parse-number sztsz)
                                                                  (parse-number s))))))))
          (if (and found (not (zerop (xarray-indexed-height found))))
            (let ((result (make-string-output-stream)))
              ;; Korábbi jogviszonyok
              (do-xarows (row r found)
                (destructuring-bind (year month day)
                    (mapcar #'(lambda (col) (xcref row col)) '(7 8 9))
                  (let ((all  (format nil "~a év ~a hó ~a nap" year month day))
                        (none (format nil "0 év 0 hó 0 nap")))
                    (destructuring-bind (classification jubilee-bonus severance-pay)
                        (mapcar #'(lambda (col) (not (empty-cell-p (xcref row col)))) '(14 11 18))
                      (apply #'format result "~a~%Jogviszony idõtartama: ~a naptól ~a napig~%Megszûnés módja: ~a~%Elismert jogcím:~%"
                             (mapcar #'(lambda (col fn)
                                         (funcall fn (xcref row col)))
                                     '(6 4 5 12)
                                     (list #'identity #'excel-date-string #'excel-date-string #'identity)))
                      (apply #'format result " - besoroláshoz: ~a~% - jubileumi jutalomhoz, felmentési idõhöz: ~a~% - végkielégítéshez: ~a~%~%"
                             (mapcar #'(lambda (valid)
                                         (if valid all none))
                                     (list classification jubilee-bonus severance-pay)))))))
              ;; Kezdõdátumok
              (apply #'format result "A közalkalmazotti jogviszonyának számított kezdõ idõpontja~% - besoroláshoz: ~a~% - jubileumi jutalomra való jogosultsághoz, felmentési idõhöz: ~a~% - végkielégítés megállapításához: ~a~%"
                     (mapcar #'excel-date-string (list date-cl date-bonus date-sever)))
              ;; Eredmény
              (get-output-stream-string result))
            "<><><>"))))

    ("elfogadom.^M^M$………………$, elektronikus"
     ,(vals-fn ((a "Vállalat hosszú megnevezése")) :obj obj
        (tks-row obj a "Helységnév")))
    ))


(defun text-template-target (temp)
  (let* ((external (carriage-return temp))
         (start    (position #\$ external))
         (end      (position #\$ external :from-end t))
         (clean    (concatenate
                    'string
                    (subseq external 0 start)
                    (subseq external (1+ start) end)
                    (subseq external (1+ end)))))
    (values clean start (1- end))))


(defun fill-template (current xarray obj)
;  (wg-msg "fill-template")
  (dolist (desc *t2*)
    (destructuring-bind (temp val-fn &optional range-fn)
        desc
      (let ((new-value (funcall val-fn xarray obj)))
        (when new-value
          (multiple-value-bind (clean start-offset end-offset)
              (text-template-target temp)
            (cclet* ((range (if range-fn
                              (funcall range-fn current)
                              (?'content current)))
                     (found (range-find-text range clean)))
              (when found
                (cclet* ((start (+ found start-offset))
                         (end   (+ found end-offset))
                         (text  (format nil "~a" new-value)))
                  (selection-overwrite range start end text))))))))))


(defun add-template (obj word filename xarray)
  ;; Új temp file dok.sablon alapján
  (let ((doctemp (doctemplate xarray obj)))
    (when doctemp
      (with-document (:doc doc :app word :open doctemp :read-only nil :close t :save t)
        (catch 'doc-exit
          (!'saveas2 doc filename)
          ;; Adatok beillesztése táblázatból
          (fill-template doc xarray obj)
          ;; Formázások
          (cclet* ((sect-trg (?'last (?'sections doc)))
                   (pri-head (!'item (?'headers sect-trg) +wd-header-footer-primary+))
                   (pg-nums  (?'pagenumbers pri-head))
                   (pg-setup (?'pagesetup sect-trg)))
            (setf (?'text (?'range pri-head)) "")               ; Meglévõ elsõdleges fejléc szövegének törlése
            (!'add pg-nums +wd-align-page-number-center+ nil)    ; Oldalszámozás középre
            (setf (?'restartnumberingatsection pg-nums) t       ; Oldalszámozás újrakezdése szakaszonként
                  (?'startingnumber pg-nums) 1                  ; Oldalszámozás kezdése 1-tõl (elsõ o. beleszámítva)
                  (?'differentfirstpageheaderfooter pg-setup) t ; Elsõ oldalon eltérõ fejléc/lábléc
                  (?'mirrormargins pg-setup) t)                 ; Tükörmargók
            (cclet* ((head  (!'item (?'headers sect-trg) +wd-header-footer-primary+))
                     (headr (?'range head)))
              (setf (?'alignment (?'paragraphformat headr)) +wd-align-paragraph-center+
                    (?'name (?'font headr)) "Times New Roman"
                    (?'size (?'font headr)) 12)))
          t))))) ; Ez kell? Ugyis visszaadnánk az elõzõ SETF értékét!


;;; Személyi kör feldolgozása, minden SZTSZ külön fájlba.
(defun process-ps (obj tk-ps-only ps word)
  ;; Iteráció SZTSZ-eken:
  (xadouniques (sztsz tk-ps-only "SZTSZ")
    (let* ((sztszp (round (parse-number sztsz)))
           (sztsz-only (xaselect tk-ps-only #'(lambda (row) (= (parse-number (xcref row "SZTSZ"))
                                                               sztszp))))
           (filename   (newfile (xarows sztsz-only 0) obj)))
      (if filename
        ;; Ha személyi körhöz van definiálva doctype:
        (disp obj "SZTSZ: ~a~a~%" sztszp
              (if (add-template obj word filename sztsz-only)
                "  ok" "  HIBA!"))
        ;; Ha személyi körhöz nincs definiálva doctype:
        (disp obj "SZTSZ: ~a   kihagyva, a ~a személyi körhöz nincs dokumentumsablon.~%" sztszp ps))
      (pstep obj)
      (pabort obj))))


(defmessenger proc-error ((err))
  "FELDOLGOZÁS: ~a~%"
  err)


(defun process (obj)
  (cclet* ((word (com:create-object :progid "Word.Application"))
           (tk-head "Vállalat hosszú megnevezése")
           (length (with-workbook (:open (get-state obj :query) :read-only t :wsvars (ws-query) :close t)
                     (length (xauniques (read-xarray (used-range ws-query)) "SZTSZ")))))
    ;; Progress bar
    (with-progress-new ("Dokumentumok generálása" obj :limit length)
      (catch 'proc-exit
        ;; Adatforrások betöltése
        (dolist (key '(:main :tks :filenum :kir :prevrels))
          (let ((filename (source-filename obj key)))
            (when (string/= filename "")
              (disp obj "Adatforrás betöltése: ~a~%" filename)
              (load-data-source obj key))))
        (disp obj "~%~%")
        (skippable (condition 'proc-exit (proc-error))
          ;; Iteráció TK-kon.
          (xadouniques  (tk (source-data obj :main) tk-head)
            (disp obj "~%~a~%~a~%~a~%~%" (line 70 #\=) (astring-upcase tk) (line 70 #\=))
            ;; Iteráció személyi körökön.
            (let ((tk-only (xaselect (source-data obj :main) #'(lambda (row) (astring= (xcref row tk-head) tk)))))
              (xadouniques (ps tk-only "SZK")
                (disp obj "~a személyi kör  ~a~%" ps (line (- 70 (+ (length ps) 15))))
                ;; Személyi kör sorok.
                (let ((tk-ps-only (xaselect tk-only #'(lambda (row) (astring= (xcref row "SZK") ps)))))
                  (process-ps obj tk-ps-only ps word))))))))
    (!'quit word)))


;;; ----------------------------------------------------------------------
;;; Main

(defun init-obj-state (obj)
  (init-state obj :doctype       (getf (first *doctypes*) :name)
                  :query         (appdir)
                  :kir-file      ""
                  :doctemp-dir   (appdir)
                  :results-dir   (appdir)
                  :tks-file      (namestring (merge-pathnames *xls-tks-filename* (appdir)))
                  :mod-start     *mod-start-default*
                  :filenum-file  ""
                  :prevrels-file ""
                  ))


;;; Dokumentumsablon-almappák ellenõrzése.
(defun temp-subdirs-found-p (doctemp-dir)
  (let* ((subdirs (mapcar #'(lambda (rec)
                              (getf rec :dir))
                          *doctypes*))
         (subdirs-found
          (mapcar #'(lambda (subdir)
                      (probe-file
                       (concatenate 'string doctemp-dir subdir)))
                  subdirs)))
    (not (member nil subdirs-found))))


(defparameter *runningp* nil) ; A "Dokumentumok generálása" gomba csak akkor indítja el a folyamatot, ha ez NIL.
(defparameter *filereq-filter-xlsx* '("Excel fájlok" "*.xlsx" "Minden fájl" "*.*"))


;;; main();
(defun start ()
  (in-package :wax)
  (let ((obj (make-instance 'wax-app :execute-fn #'process)))
    (init-obj-state obj)
    (load-state obj)
    ;; Fõablak létrehozása
    (wg-window
     "Kinevezés generáló 2026.01.01."
     180
     
     "Dokumentumtípus választása"
     (wg-options
      #'(lambda (text &rest rest)
          (declare (ignore rest))
          (setf (get-state obj :doctype) text))
      (mapcar #'(lambda (rec)
                  (getf rec :name))
              *doctypes*)
      (get-state obj :doctype))
     
     "Tanév/módosítás érvényesség kezdete";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (wg-text-input 
      #'(lambda (text &rest rest)
          (declare (ignore rest))
          (setf (get-state obj :mod-start) text))
      (get-state obj :mod-start))
     
     "SAP lekérdezés eredménye";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (wg-file-selector
      "SAP lekérdezés eredménye"
      (second *filereq-filter-xlsx*)
      *filereq-filter-xlsx*
      #'(lambda (text &rest rest)
          (declare (ignore rest))
          (setf (get-state obj :query) text))
      (get-state obj :query))
     
#|     "Elõzõ jogviszonyok (opcionális)";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (wg-file-selector
      "Elõzõ jogviszonyok"
      (second *filereq-filter-xlsx*)
      *filereq-filter-xlsx*
      #'(lambda (text &rest rest)
          (declare (ignore rest))
          (setf (get-state obj :prevrels-file) text)
;          (when (string= text "")
;            (setf *fileno-data* nil))
          )
      (get-state obj :prevrels-file)
      :cancel #'(lambda () (setf (get-state obj :prevrels-file) ""
                                 ;*fileno-data* nil
                                 )))|#
     
     "Iktatószámok listája (opcionális)";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (wg-file-selector
      "Iktatószámok listája"
      (second *filereq-filter-xlsx*)
      *filereq-filter-xlsx*
      #'(lambda (text &rest rest)
          (declare (ignore rest))
          (setf (get-state obj :filenum-file) text)
;          (when (string= text "")
;            (setf *fileno-data* nil))
          )
      (get-state obj :filenum-file)
      :cancel #'(lambda () (setf (get-state obj :filenum-file) ""
;                            *fileno-data* nil
                            )))
     
     "KIR feladatellátási helyek (opcionális)";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (wg-file-selector
      "KIR feladatellátási helyek listája"
      (second *filereq-filter-xlsx*)
      *filereq-filter-xlsx*
      #'(lambda (text &rest rest)
          (declare (ignore rest))
          (setf (get-state obj :kir-file) text)
;          (when (string= text "")
;            (setf *kir-data* nil))
          )
      (get-state obj :kir-file)
      :cancel #'(lambda () (setf (get-state obj :kir-file) ""
;                                 *kir-data* nil
                                 )))
     
     "Dokumentumsablonok mappája";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (wg-dir-selector
      "Dokumentumsablonok mappája"
      #'(lambda (text &rest rest)
          (declare (ignore rest))
          (setf (get-state obj :doctemp-dir) text
                (get-state obj :tks-file) (namestring (merge-pathnames *xls-tks-filename* text))))
      (get-state obj :doctemp-dir))
     
     "Generált dokumentumok mappája";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (wg-dir-selector
      "Generált dokumentumok mappája"
      #'(lambda (text &rest rest)
          (declare (ignore rest))
          (setf (get-state obj :results-dir) text))
      (get-state obj :results-dir))

     (wg-button
      "Dokumentumok generálása";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      #'(lambda (interface)
          (declare (ignore interface))
          ;; Ha dok.sablon almappák megvannak, indítás, egyébként figyelmeztetés.
          (if (not (temp-subdirs-found-p (get-state obj :doctemp-dir)))
            (wg-msg "A dokumentumsablonok kiválasztott mappája érvénytelen!~%Kérem szíveskedjen azt a mappát kiválasztani, amelyik az \"Egyoldalú kinevezésmódosítások\", \"Kétoldalú kinevezésmódosítások\" és \"Kinevezések\" almappákat tartalmazza.")
            (unless *runningp*
              ;; Ha még nem fut, indítás.
              (let ((*runningp* t))
                (wg-floating-message "Indítás ...")
                ;; State mentése
                (save-state obj)
                ;; Adatforrások felvétele.
                (mapc #'(lambda (src var) (add-data-source obj src (get-state obj var)))
                      '(:main  :filenum      :kir      :prevrels)
                      '(:query :filenum-file :kir-file :prevrels-file))
                (add-data-source obj :tks (namestring
                                           (merge-pathnames *xls-tks-filename*
                                                            (get-state obj :doctemp-dir))))
                ;; Szkript végrehajtása.
                (wax-execute obj :errorsink-on nil)
                ;; Adatforrások eldobása.
                (dolist (key '(:main :tks :filenum :kir :prevrels))
                  (remove-data-source obj key))))))))))



;;; ----------------------------------------------------------------------
;;; Sandbox












#.(disable-ccom-syntax)

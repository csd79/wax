;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)


;; ----------------------------------------------------------------------
;; Global vars


(defparameter *s* "c:\\Users\\cselovszkid\\common-lisp\\wax\\teszt.docx")
(defparameter *so* "c:\\Users\\cselovszkid\\common-lisp\\wax\\teszt__.docx")


(defparameter *eol* (format nil "~C" #\return))


#|
2 üzemmód kell:

  1. Egyszeri feldolgozás. A program végigmegy az inline jelzõkön, összegyûjti
  a kifejezéseket és a hozzájuk tartozó pozíciókat, majd mindent kiértékel, és
  az értékeket becseréli a megjegyzett pozíciókra. Ezután az eredményt új
  néven elmenti.

  2. Iteratív feldolgozás. Standard globális változókban megadott helyen
  (Excel fájl, munkalap, oszlop: *ITER-FILE*, *ITER-SHEET*, *ITER-HEADER*)
  lévõ egyedi értékeken megy végig, az aktuális értéket egy standard globális
  változóban tárolja: *CURRENT*.

Módválasztás: ha az *ITER-FILE*, *ITER-SHEET* és *ITER-HEADER* változók
definiálva vannak, akkor iteratív mód szükséges, egyébként egyszeri.

Mentés: kell egy standard globális változó ami a mentett fájl nevét
tartalmazza: *OUTFILE*. Ha ez üres, akkor a program a bemeneti fájl alapján
generál egy nevet.

Szintaxis: (Excelhez újra kell gondolni!)

  #&           Az ezután következõ kifejezés ki lesz értékelve, de az értéke
               nem kerül behelyettesítésre a dokumentumba.

  #;           Az ez után következõ szöveg a sor végéig megjegyzés. (Más
               kifejezések után ; nem használható, csak ilyen módon.)

  #@           Az ezután következõ kifejezés értéke behelyettesítésre kerül
               a dokumentumba.

  LOCAL        Mint LET*, de minden késõbb következõ beszúrás a hatókörébe
               fog számítani.

  #{/#}        Töbsoros megjegyzés



|#


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

(defun remove-simple-comments (string)
  (remove-delimited-substrings string "#;" *eol*))

(defun remove-multiline-comments (string)
  (remove-delimited-substrings string "#{" "#}"))


(defun search-any (subseqs sequence &key (test #'equalp))
  ;; Trying to find first occurance of each subseq
  (let ((positions  (mapcar #'(lambda (subseq)
                                (search subseq sequence :test test))
                            subseqs))
        (min-pos    nil)
        (min-subseq nil))
    ;; Determining first occurance of any subseq
    (mapc #'(lambda (subseq position)
              (if min-pos
                ;; There were previous finds
                (when (< position min-pos)
                  (setf min-pos position
                        min-subseq subseq))
                ;; First find
                (when position
                  (setf min-pos position
                        min-subseq subseq))))
          subseqs positions)
    (values min-pos min-subseq)))
    

(defun decorate (symbol)
  (format nil "#[~a]" (symbol-name symbol)))


(defun replace-1substring (string old new &optional (start 0))
  (let ((start (search old string :start2 start)))
    (if start
        (let* ((end    (+ start (length old)))
               (before (subseq string 0 start))
               (after  (subseq string end)))
          (concatenate 'string before new after))
      string)))


(defun doc-processor (string &optional (exp-pairs '()))
  ;; Look for inserts
  (multiple-value-bind (start subseq)
      (search-any '("#&" "#@") string)
    (if start
      ;; If found, add expression to EXP-PAIRS
      (multiple-value-bind (expression next)
          (read-from-string string nil nil
                            :start (+ start (length subseq))
                            :preserve-whitespace t)
        (let* ((symbol        (gensym))
               (new-exp-pairs (append exp-pairs
                                      (list symbol expression)))
               (exp-string    (subseq string start next))
               (new-string    (cond
                               ;; Insert = #@: replace it decorated symbol
                               ((string= subseq "#@")
                                (replace-1substring string
                                                    exp-string
                                                    (decorate symbol)))
                               ;; Insert = #&: remove it with trailing newline
                               ((string= subseq "#&")
                                (replace-1substring string
                                                    (if (char= (elt string next)
                                                               (elt *eol* 0))
                                                      (concatenate 'string
                                                                   exp-string *eol*)
                                                      exp-string)
                                                    ""))
                               ;; Default case, should never occur
                               (t string))))
          (doc-processor new-string new-exp-pairs)))
      ;; No more inserts
      (values string exp-pairs))))




(defun test1 ()
  (cclet* ((document (get-document *s*))
           (content  #p(content document)))
    #p(text content)))

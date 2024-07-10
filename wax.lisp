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

  #@           Az ezután következõ kifejezés értéke behelyettesítésre kerül
               a dokumentumba.

  #&(LOCAL     Mint CCLET*, de minden késõbb következõ beszúrás a hatókörébe
               fog számítani. #@ beszúrással nem mûködik!

  #;           Az ez után következõ szöveg a sor végéig megjegyzés. (Más
               kifejezések után ; nem használható, csak ilyen módon.)

  #{/#}        Többsoros megjegyzés



|#


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
;          (print exp-string)
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
      (list 'cclet* (apply #'append (nreverse head))
            (append (list 'let* neck)
                    body)))))


(defconstant +wd-find-continue+  1)

;;; Replace text in Word doc.
(defun word-replace-text (find orig-text new-text)
  #m(execute find orig-text nil nil nil nil nil t +wd-find-continue+ nil new-text))


;;; ------------------------


(defun test1 ()
  (cclet* ((document (get-document *s*))
           (content  #p(content document)))
    #p(text content)))


(defun test2 ()
  (cclet* ((word      (com:create-object :progid "Word.Application"))
           (documents #p(documents word))
           (document  #m(open documents *s*))
           (content   #p(content document))
           (text      #p(text content))
           (find      #p(find content))) 
    (let ((comments (list-comments text)))
      (multiple-value-bind (exp-pairs repl-subs remove-subs)
          (doc-processor text)
;        (pprint (rearrange exp-pairs))))))
;        (pprint repl-subs)))))
        (pprint remove-subs)))
    #m(close document)))


#|(defun remove-comments (document)
  (cclet* ((content  #p(content document))
           (text     #p(text content))
           (find     #p(find content))
           (comments (list-comments text)))
    (dolist (comment comments)
      (word-replace-text find comment ""))
    document))|#


(defun test3 ()
  (cclet* ((word      (com:create-object :progid "Word.Application"))
           (documents #p(documents word))
           (document  #m(open documents *s*))
           (content  #p(content document))
           (text     #p(text content))
           (find     #p(find content))
           (comments (list-comments text)))
    (unwind-protect
        (progn
          ;; Remove comments
          (dolist (comment comments)
            (word-replace-text find comment ""))
          (multiple-value-bind (expr-clauses placeholders removables)
              (doc-processor text)
            ;; Remove #&s
            (dolist (removable removables)
              (word-replace-text find removable ""))
            ;; Construct body
            (let* ((body  (mapcar 
                           #'(lambda (placeholder)
                               (append '(word-replace-text find) placeholder))
                           placeholders))
                   (whole (construct-fn expr-clauses body)))
              (progv '(find) (list find)
                (eval whole)))
;            (search-file '(1 2 3) "vazz" 1 "ok")
            ))
      (progn
        #m(saveas2 document *so*)
        #m(close document 0)))))

;;; A VÉGÉN ÉRDEMES LENNE FELSZABADÍTANI A SOK GENSYM-ET, MERT ITERATÍV MÓDBEN RENGETEG LESZ.
;;; VAGY MAGÁTÓL MEGY??


;;; MI VAN HA HEAD VAGY NECK ÜRES??? AKKOR CONSTRUCT-FN MIT CSINÁL???


;;; A WORD REPLACE-BEN TÉNYLEG VAN EGY LIMIT A (content  #p(content document))
;;; KÓDBLOKK KIAKASZTJA. DARABOLÁS?
;;; A hiba a WORD-REPLACE-TEXT-ben jelentkezik, 255 karakternél hosszabb csere esetén.
;;; Itt trükk kell, hogy keresse meg a végét és töröljön, és csak az elejére (<255 kar) alkalmazza a cserét.



;;; Ha valahol kimarad egy záró zárójel, a read-from-string ráfuthat egy insert kódra #& és rögtön kiabál hogy nincs ilyen reader karakter.
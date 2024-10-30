;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*-

(in-package :wax)


;; ----------------------------------------------------------------------
;; History


(defparameter *independent-exe* nil)


(defun appdir ()
  (if *independent-exe*
      (namestring (lw:current-pathname))
    "c:\\Users\\cselovszkid\\common-lisp\\wax\\"))


(defun appfile (file)
  (merge-pathnames file (appdir)))


(defun load-forms (file)
  (let ((results '()))
    (with-open-file (in file
                        :direction :input
                        :if-does-not-exist nil)
      (when in
        (loop for f = (read in nil)
              until (null f)
              doing (push f results)))
      (nreverse results))))


(defun save-forms (file &rest forms)
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (dolist (form forms)
      (prin1 form out))))
;      (format out "~a~%" form))))


;; ----------------------------------------------------------------------
;; Plists


;; Build a copy of PLIST, but remove every pair whose
;; key is not listed in KEYS-TO-KEEP.
(defun keep-pairs-worker (plist keys-to-keep &optional (acc '()))
  (multiple-value-bind (key val rest)
      (get-properties plist keys-to-keep)
    (if key
        (keep-pairs-worker (cddr rest) keys-to-keep (append (list key val) acc))
      acc)))

;; Return a copy of PLIST, but remove every pair whose
;; key is not listed in KEYS-TO-KEEP. If KEYS-TO-KEEP
;; is empty, return PLIST unchanged.
(defun keep-pairs (plist &optional (keys-to-keep '()))
  (if keys-to-keep
      (keep-pairs-worker plist keys-to-keep)
    plist))

;; Return a copy of PLIST, but remove every pair whose
;; key is a member of KEYS-TO-REMOVE.
(defun remove-pairs (plist &optional (keys-to-remove '()))
  (if keys-to-remove
      (loop for (key value) on plist by #'cddr
            unless (member key keys-to-remove)
            collect (list key value) into result
            finally (return (apply #'nconc result)))
    plist))

;; Return a copy of PLIST with the keys modified
;; according to the contents of KEYS-TO-MODIFY (:old-key :new-key ...)
(defun modify-keys (plist &optional (keys-to-modify '()))
  (if keys-to-modify
      (loop for (key value) on plist by #'cddr
            collect (list (or (getf keys-to-modify key)
                              key)
                          value) into result
            finally (return (apply #'nconc result)))
    plist))
                                     
;; Append NEW-PAIRS to PLIST, overriding any pairs with the same key.
(defun override-pairs (plist &optional (new-pairs '()))
  (if new-pairs
      (let* ((keys-to-override (loop for (key) on new-pairs by #'cddr
                                     collecting key))
             (pruned-plist (loop for (key value) on plist by #'cddr
                                 unless (member key keys-to-override)
                                 collect (list key value) into result
                                 finally (return (apply #'nconc result)))))
        (append pruned-plist new-pairs))
    plist))

;; Remove any pairs whose value is nil.
(defun drop-nils (plist)
  (loop for (key value) on plist by #'cddr
        unless (null value)
        collect (list key value) into list1
        finally (return (apply #'nconc list1))))

;; Return modified copy of PLIST using the above fns.
(defun modify-plist (plist &key (keep '()) (remove '()) (modify '()) (override '()) (drop-nils nil))
  (let* ((result1 (keep-pairs plist keep))
         (result2 (remove-pairs result1 remove))
         (result3 (modify-keys result2 modify))
         (result4 (override-pairs result3 override)))
    (if drop-nils
        (drop-nils result4)
      result4)))

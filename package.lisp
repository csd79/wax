;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:cl-user)


(defpackage #:wax
  (:use #:cl #:ccom #:achar)
  (:export
   #:start
   #:appdir
   #:appfile
   #:load-forms
   #:save-forms
   #:keep-pairs
   #:remove-pairs
   #:modify-keys
   #:override-pairs
   #:drop-nils
   #:modify-plist
   #:with-progress
   #:wg-text-input
   #:wg-file-selector
   #:wg-dir-selector
   #:wg-options
   #:wg-button
   #:wg-window
   #:wg-msg
   #:wg-floating-message
   #:wg-confirm
   #:with-wax-errorsink
   #:empty-cell-p
   #:timestamp
   #:clean-city
   #:clean-name
   #:add-article
   #:sub->words
   #:currency
   #:identify-month
   #:parse-hudate
   #:hudate->unitime))

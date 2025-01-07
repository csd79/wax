;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:cl-user)


(defpackage #:wax
  (:use #:cl #:ccom #:achar)
  (:export
   #:start
   #:appdir
   #:user-homedir
   #:user-tempdir
   #:random-alphanumeric-string
   #:new-temp-filename
   #:saving-with-intermediate-temp
   #:appfile
   #:load-forms
   #:save-forms
   #:hide-file
   #:keep-pairs
   #:remove-pairs
   #:modify-keys
   #:override-pairs
   #:drop-nils
   #:modify-plist
   #:timestamp
   #:identify-month
   #:parse-hudate
   #:hudate->unitime
   #:sub->words
   #:currency
   #:clean-city
   #:remove-illegal-filename-chars
   #:clean-name
   #:add-article
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
   #:errorsink-on
   #:with-wax-errorsink
))

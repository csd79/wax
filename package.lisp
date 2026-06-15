;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:cl-user)


(defpackage #:wax
  (:use #:cl #:ccom4 #:achar #:ccoffice)
  (:export
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
   #:valid-date-p
   #:hudate
   #:hudate-parsable
   #:sub->words
   #:currency
   #:clean-city
   #:remove-illegal-filename-chars
   #:clean-name
   #:add-article
   #:fix-phone-number

   #:eval-definition
   #:lambda-expr
   #:compiled-fn

   #:line

   #:with-progress
   #:with-progress-new

   #:wg-text-input
   #:wg-file-selector
   #:wg-dir-selector
   #:wg-options
   #:wg-button
   #:wg-window
   #:wg-msg
   #:wg-floating-message
   #:wg-confirm

   #:backtrace->string
   #:defmessenger
   #:skippable
   #:errorsink-on
   #:skip
   #:with-wax-errorsink

   #:data-source
   #:filename
   #:data
   #:loaded-p
   #:load-src
   #:purge
   #:select-row
   #:wax-app
   #:state
   #:execute-fn
   #:disp-fn
   #:pstep-limit
   #:pstep-fn
   #:pabort-fn
   #:data-sources
   #:disp
   #:set-pstep-limit
   #:pstep
   #:pabort
   #:save-state
   #:load-state
   #:init-state
   #:get-state
   #:trim-state
   #:load-descriptives
   #:add-data-source
   #:remove-data-source
   #:load-data-source
   #:purge-data-source
   #:select-row-from
   #:source-filename
   #:source-data
   #:set-execute-fn
   #:init-wax-app
   #:wax-execute
   ))
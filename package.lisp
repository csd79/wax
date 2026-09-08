;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:cl-user)


(defpackage #:wax
  (:use #:cl #:utils #:ccom4 #:achar #:ccoffice)
  (:export

   #:with-progress
   #:with-progress-new

   #:wg-text-input
   #:wg-file-selector
   #:wg-dir-selector
   #:wg-options
   #:wg-button
   #:wg-window
   #:wg-login
   #:wg-msg
   #:wg-floating-message
   #:wg-confirm

   #:text-stream
   #:step-progress
   #:abort-progress-when-requested
   #:progress
   #:kill-progress
   #:with-progress-window
   
   #:backtrace->string
   #:defmessenger
   #:skippable
;   #:errorsink-on
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
   #:errorsink-enabled-p
   #:execute-fn
   #:disp-fn
   #:pstep-limit
   #:pstep-fn
   #:pabort-fn
   #:data-sources

;   #:disp
;   #:set-pstep-limit
;   #:pstep
;   #:pabort

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
   #:enable-errorsink
   #:disable-errorsink
   #:wax-execute
   ))
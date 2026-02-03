;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)
(require "shell-objs")


;; ----------------------------------------------------------------------
;; Progress window


#|(capi:define-interface progress ()
  ()
  (:panes
   ;; Pane to contain text output from the working thread.
   (text-disp
    capi:editor-pane
    :accessor           text-disp
    :flag               'minimal-example
    :buffer-name        (:initarg buffer-name)
    :enabled            :read-only
    :visible-min-width  '(character 80)
    :visible-min-height '(character 30)
    :vertical-scroll    t)
   ;; Progress bar.
   (progress
    capi:progress-bar
    :accessor progress
    :start 0
    :end 100)
   (rest-time
    capi:title-pane
    :accessor rest-time
    :text "")
   (abort-button
    capi:push-button
    :accessor abort-button
    :text "Megszakítás"
    :callback-type :interface
    :callback (:initarg abort-callback)))
  (:default-initargs
   :title "Feldolgozás"
   :best-x 735
   :best-y 400
   :window-styles '(:borderless
                    :shadowed
                    :movable-by-window-background)))|#
(capi:define-interface progress ()
  ()
  (:panes
   ;; Pane to contain text output from the working thread.
   (text-disp
    capi:editor-pane
    :accessor           text-disp
    :flag               'minimal-example
    :buffer-name        (:initarg buffer-name)
    :enabled            :read-only
    :visible-min-width  '(character 80)
    :visible-min-height '(character 30)
    :wrap-style         :split-on-space
    :vertical-scroll    t)
   ;; Progress bar.
   (progress
    capi:progress-bar
    :accessor progress
    :start 0
    :end 100)
   (rest-time
    capi:title-pane
    :accessor rest-time
    :text "")
   (abort-button
    capi:push-button
    :accessor abort-button
    :text "Megszakítás"
    :callback-type :interface
    :callback (:initarg abort-callback))
   (close-button
    capi:push-button
    :accessor close-button
    :text "Bezárás"
    :callback-type :interface
    :callback (:initarg close-callback)
    :enabled nil)
   )
  (:layouts
   (upper capi:column-layout '(text-disp progress rest-time))
   (lower capi:row-layout '(abort-button close-button))
   (window capi:column-layout '(upper lower)))
  (:default-initargs
   :title "Feldolgozás"
   :best-x 735
   :best-y 400
   :layout 'window
   :confirm-destroy-function (lambda (interface)
                               (capi:button-enabled (close-button interface)))
#|                                 (if (capi:button-enabled (close-button interface))
                                   (progn
                                     (push "T" sig::g)
                                     t)
                                   (progn
                                     (let ((fn (capi:button-press-callback (abort-button interface))))
                                       (funcall (capi:button-press-callback (abort-button interface))
                                                interface)
                                       (push fn sig::g))
                                     nil)))|#
   :window-styles '(
                    ;:borderless
                    ;:shadowed
                    ;:movable-by-window-background
                    )))


(defun timestr (secs)
  (let* ((hours (truncate (/ secs 3600)))
         (rem1  (- secs (* hours 3600)))
         (mins  (truncate (/ rem1 60)))
         (secs  (- rem1 (* mins 60)))
         (accum '()))
    (unless (zerop hours)
      (push (format nil "~d:" hours) accum))
    (push (format nil "~2,'0d:~2,'0d" mins secs) accum)
    (apply #'concatenate 'string (nreverse accum))))


#|(defmacro with-progress ((title abort disper mover ccount &optional (buffername "temp")) &body body)
  (let ((interface  (gensym))
        (i          (gensym))
        (aborted    (gensym))
        (start-time (gensym))
        (count      (gensym)))
    `(progn(timestamp (get-universal-time))
       (let* ((,aborted   nil)
              (,interface (make-instance 'progress :title ,title :buffer-name ,buffername
                                         :abort-callback #'(lambda (interface)
                                                             (declare (ignore interface))
                                                             (when (wg-confirm "Megszakítja a feldolgozást?")
                                                               (wg-floating-message "Megszakítás ...")
                                                               (setf ,aborted t)))))
              (,i 0)
              (,count ,ccount)
              (,start-time (get-internal-real-time)))
         (unwind-protect
             (progn
               (capi:modify-editor-pane-buffer (text-disp ,interface) :contents "")
               (capi:display ,interface)
               (block big-body
                 (flet ((,mover (&key (abs nil) (step 1))
                          (let* ((percent (if (and abs (numberp abs) (<= abs 100))
                                            abs
                                            (* 100 (/ (incf ,i step) ,count))))
                                 (current-time (get-internal-real-time))
                                 (time-spent   (/ (- current-time ,start-time)
                                                  internal-time-units-per-second))
                                 (time-left    (- (* time-spent (/ 100 percent)) time-spent)))
                            (setf (capi:range-slug-start (progress ,interface)) (round percent))
                            (setf (capi:title-pane-text (rest-time ,interface))
                                  (format nil "Eltelt idõ: ~a,  becsült hátralévõ idõ: ~a"
                                          (timestr (round time-spent))
                                          (timestr (round time-left))))))
                        (,disper (string &rest args)
                          (ignore-errors
                            (let* ((buffer (editor:buffer-from-name ,buffername))
                                   (point  (editor:buffers-end buffer)))
                              (editor:insert-string point (apply #'format nil string args))
                              (capi:scroll (text-disp ,interface) :vertical :move :end))))
                        (,abort ()
                          (when ,aborted
                            (return-from big-body))))
                   ,@body
                   (wg-msg "A feldolgozás véget ért.")
                   )))
           (capi:destroy ,interface))))))|#


(defun switch-buttons (interface)
  (setf (capi:button-enabled (close-button interface)) t
        (capi:button-enabled (abort-button interface)) nil))

(defparameter *faces*
  (list
   (editor:make-face 'one   :if-exists :overwrite :foreground :red :bold-p t)
   (editor:make-face 'two   :if-exists :overwrite :foreground :honeydew4 :italic-p t)
   (editor:make-face 'three :if-exists :overwrite :foreground :blue3 :underline-p t)
   ))
        


(defun face ()
  (nth (random (length *faces*)) *faces*))


(defmacro with-progress-new ((title obj &key (limit nil) (buffername "temp")) &body body)
  (let ((interface  (gensym))
        (i          (gensym))
        (aborted    (gensym))
        (start-time (gensym))
        (count      (gensym))
        (rollback   (gensym)))
    `(progn
       (let* ((,aborted   nil)
              (,interface
               (make-instance 'progress
                              :title ,title
                              :buffer-name ,buffername
                              :abort-callback #'(lambda (interface)
                                                  (when (and (not ,aborted)
                                                             (wg-confirm "Megszakítja a feldolgozást?"))
                                                    (wg-floating-message "Megszakítás ...")
                                                    (setf ,aborted t)
                                                    (switch-buttons interface)
                                                    ))
                              :close-callback #'(lambda (interface)
                                                  (capi:destroy interface))))
              (,i 0)
              (,count ,limit)
              (,start-time (get-internal-real-time))
              (,rollback nil))
         (capi:modify-editor-pane-buffer (text-disp ,interface) :contents "")
         (capi:display ,interface)
         (block big-body
           (setf (pstep-fn ,obj)
                 #'(lambda (&key (abs nil) (step 1))
                     (let* ((percent (if (and abs (numberp abs) (<= abs 100))
                                       abs
                                       (* 100 (/ (incf ,i step)
                                                 (or ,count (pstep-limit ,obj))))))
                            (current-time (get-internal-real-time))
                            (time-spent   (/ (- current-time ,start-time)
                                             internal-time-units-per-second))
                            (time-left    (max (- (* time-spent (/ 100 percent)) time-spent)
                                               0)))
                       (setf (capi:range-slug-start (progress ,interface)) (round percent))
                       (setf (capi:title-pane-text (rest-time ,interface))
                             (format nil "Eltelt idõ: ~a,  becsült hátralévõ idõ: ~a"
                                     (timestr (round time-spent))
                                     (timestr (round time-left))))))
                 (disp-fn ,obj)
                 #'(lambda (string &rest args)
                     (ignore-errors
#|                       (let* ((buffer (editor:buffer-from-name ,buffername))
                              (point  (editor:buffers-end buffer)))
                         (editor:insert-string point (apply #'format nil string args))
                         (capi:scroll (text-disp ,interface) :vertical :move :end))|#
                       (let* ((buffer   (editor:buffer-from-name ,buffername))
                              (point    (editor:buffers-end buffer))
                              (formated (apply #'format nil string args)))
                         (editor:with-point ((start point :before-insert)
                                             (end   point :after-insert))
                           (editor:insert-string start formated)
;                           (editor:insert-string start (editor:points-to-string start end))

;                           (editor:put-text-property-no-edit start end 'face (face))
                           (editor:put-text-property-no-edit
                            (editor:buffers-start buffer)
                            (editor:buffers-end buffer)
                            'face (face))

                           (capi:scroll (text-disp ,interface) :vertical :move :end)
                           )
                         )
                       ))
                 (pabort-fn ,obj)
                 #'(lambda ()
                     (when ,aborted
                       (disp ,obj "~%~%A feldolgozás megszakítva, az ablak bezárható.~%")
                       (return-from big-body)))
                 (pkill-fn ,obj)
                 #'(lambda ()
                     (setf ,aborted t)
                     (disp ,obj "~%~%A feldolgozás félbeszakadt, az ablak bezárható.~%")
                     (switch-buttons ,interface))
                 )
           ,@body
           (when (errorlogs-waiting-p ,obj)
             (setf ,rollback (capi:get-vertical-scroll-parameters (text-disp ,interface) :max-range))
             (let ((line (line 70 #\*)))
               (disp ,obj "~3%~a~%HIBÁK RÉSZLETEZÉSE:~%~a~2%" line line))
             (disp-errorlogs ,obj)
             (purge-errorlogs ,obj))
           (switch-buttons ,interface)
           (disp ,obj "~2%A feldolgozás befejezõdött, az ablak bezárható.~%")
           (when ,rollback
             (capi:execute-with-interface
              ,interface
              #'(lambda ()
                  (capi:scroll (text-disp ,interface) :vertical :move ,rollback)))))))))


;; ----------------------------------------------------------------------
;; Main window


(defun wg-text-input (callback text &rest rest)
  (apply #'make-instance
         (append (list 'capi:text-input-pane
                       :text text
                       :callback callback
                       :change-callback callback)
                 rest)))


(defun wg-text-input2 (callback text completion-fn)
  (wg-text-input callback text
                 :buttons `(:ok nil :completion t :cancel t
                            :cancel-function ,#'(lambda (pane) (setf (capi:text-input-pane-text pane) text)))
                 :completion-function completion-fn))


(defun wg-file-selector (message filter filters callback text &key (cancel nil))
  (make-instance
   'capi:text-input-pane
   :text text
   :buttons `(:browse-file
              (:message ,message
               :pathname ,(if (string= text "") (appdir) text)
               :if-does-not-exist :error
               :filter ,filter
               :filters ,filters)
              :ok nil
              :cancel ,cancel
              :cancel-function ,#'(lambda (pane)
                                    (setf (capi:text-input-pane-text pane) "")
                                    (when cancel
                                      (funcall cancel))))
   :text-change-callback callback))


(defun wg-dir-selector (message callback text)
  (make-instance
   'capi:text-input-pane
;   :title label
   :text text
   :buttons `(:browse-file
              (:message ,message
               :pathname ,(if (string= text "") (appdir) text)
               :directory t
               :if-does-not-exist :error
               :use-file-dialog t)
              :ok nil)
;   :callback callback
;   :editing-callback callback
;   :change-callback callback
   :text-change-callback callback
   ))


(defun wg-options (callback items item)
  (make-instance
   'capi:option-pane
;   :title label
   :items items
   :selection (position item items :test #'string=)
   :selection-callback callback))


(defun wg-button (label callback)
  (make-instance
   'capi:push-button
   :text label
   :callback-type :interface
   :callback callback))


(defun wg-checkbox (label callback &optional (default nil))
  (make-instance
   'capi:check-button
   :text label
   :callback-type :element
   :selection-callback (lambda (element)
                         (funcall callback (capi:button-selected element)))
   :retract-callback (lambda (element)
                       (funcall callback (capi:button-selected element)))
   :selected default))


(defun wg-window (title max-height &rest list)
  (capi:contain
   (make-instance
    'capi:grid-layout
    :rows (ceiling (/ (length list) 2))
    :description list)
   :best-x '(- (/ :screen-width 2) 200)
   :best-y '(- (/ :screen-height 2) 100)
   :best-width 650
   :max-height max-height
   :title title))


(defun wg-msg (string &rest rest)
  (apply #'capi:display-message string rest))


(defun wg-floating-message (string &optional (timeout 4))
  (capi:display-non-focus-message string :timeout timeout))


(defun wg-confirm (string &rest rest)
  (funcall #'capi:confirm-yes-or-no string rest))


;; ----------------------------------------------------------------------
;; Error dialog


(defparameter *wg-error-details* nil)


(defun wg-save-error (&rest interface)
  (declare (ignore interface))
  (let* ((dir  (capi:prompt-for-directory 
                "Válassza ki a mappát a hibajelzés mentéshez"
                :use-file-dialog t
                :pathname (appdir)))
         (file (when dir
                 (make-pathname :defaults dir
                                :name (concatenate 'string "error-" (timestamp (get-universal-time)))
                                :type "txt"))))
    (when file
      (save-forms file *wg-error-details*)
      (setf *wg-error-details* nil)
      (wg-msg "A hibajelzés elmentve:~%~a" file))))


(gp:register-image-translation
 'utya-duck
  (gp:read-external-image (concatenate 'string (appdir) "img\\utya-duck.bmp")))


(defun display-utya-duck (pane x y width height)
  (let ((image (gp:load-image pane 'utya-duck)))
    (gp:draw-image pane image 0 0)))


(capi:define-interface errordial ()
  ()
  (:panes
   (utya-duck
    capi:output-pane
    :display-callback 'display-utya-duck
    :visible-min-width 176
    :visible-min-height 200
    )
   (error-message
    capi:display-pane
    :accessor error-message
    :text (:initarg message)
    :visible-border nil
    :background :gray
    :visible-max-width 280
;    :internal-max-width 280
;    :external-max-width 280
    :visible-max-height 200
    )
   (two-buttons
    capi:push-button-panel
    :accessor two-buttons
    :items (list "Hibajelzés mentése" "Bezárás")
    :layout-args '(:x-uniform-size-p t)
    :callback-type :interface
    :callbacks '(wg-save-error capi:quit-interface)))
  (:layouts
   (upper
    capi:row-layout
    '(utya-duck
      error-message
      )
    :x-adjust :left
;    :y-adjust :centre
    )
   (lower
    capi:row-layout
    '(two-buttons)
;    :x-adjust :centre
    )
   (rows
    capi:column-layout
    '(upper
      lower
      )
    :x-adjust :left
;    :visible-max-width 490
    ))
  (:default-initargs
   :title "Váratlan esemény"
   :best-x '(- (/ :screen-width 2) 200)
   :best-y '(- (/ :screen-height 2) 100)
   :best-width 400
   :visible-max-width 400
   :internal-max-width 400
   :external-max-width 400
;   :best-height 200
   :layout 'rows
;   :resizable nil
   ))


(defun wg-errordial (message details)
  (setf *wg-error-details* (append (list :message message) details))
  (capi:contain
   (make-instance
    'errordial
    :message message)))

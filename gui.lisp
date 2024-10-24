;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)
(require "shell-objs")


;; ----------------------------------------------------------------------
;; Progress window


(capi:define-interface progress ()
  ()
  (:panes
   ;; Pane to contain text output from the working thread.
   (text-dump
    capi:editor-pane
    :accessor           text-dump
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
                    :movable-by-window-background)))


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


(defmacro with-progress ((title abort dumper mover ccount &optional (buffername "temp")) &body body)
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
               (capi:modify-editor-pane-buffer (text-dump ,interface) :contents "")
               (capi:display ,interface)
               (block big-body
                 (flet ((,mover (&optional (n nil))
                          (let* ((percent (if (and n (numberp n) (<= n 100))
                                            n
                                            (* 100 (/ (incf ,i) ,count))))
                                 (current-time (get-internal-real-time))
                                 (time-spent   (/ (- current-time ,start-time)
                                                  internal-time-units-per-second))
                                 (time-left    (- (* time-spent (/ 100 percent)) time-spent)))
                            (setf (capi:range-slug-start (progress ,interface)) (round percent))
                            (setf (capi:title-pane-text (rest-time ,interface))
                                  (format nil "Eltelt idõ: ~a,  becsült hátralévõ idõ: ~a"
                                          (timestr (round time-spent))
                                          (timestr (round time-left))))))
                        (,dumper (string &rest args)
                          (ignore-errors
                            (let* ((buffer (editor:buffer-from-name ,buffername))
                                   (point  (editor:buffers-end buffer)))
                              (editor:insert-string point (apply #'format nil string args))
                              (capi:scroll (text-dump ,interface) :vertical :move :end))))
                        (,abort ()
                          (when ,aborted
                            (return-from big-body))))
                   ,@body
                   (wg-msg "A feldolgozás véget ért."))))
           (capi:destroy ,interface))))))


;; ----------------------------------------------------------------------
;; Main window


(defun wg-text-input (label callback text)
  (make-instance
   'capi:text-input-pane
   :title label
   :text text
   :callback callback
   :change-callback callback))


(defun wg-file-selector (label filter filters callback text)
  (make-instance
   'capi:text-input-pane
   :title label
   :text text
   :buttons `(:browse-file
              (:if-does-not-exist :error
               :filter ,filter
               :filters ,filters)
              :ok nil)
   :callback callback
   :change-callback callback))


(defun wg-dir-selector (label callback text)
  (make-instance
   'capi:text-input-pane
   :title label
   :text text
   :buttons `(:browse-file
              (:directory t
               :if-does-not-exist :error
               :use-file-dialog t)
              :ok nil)
   :callback callback
   :change-callback callback))


(defun wg-options (label callback items item)
  (make-instance
   'capi:option-pane
   :title label
   :items items
   :selection (position item items :test #'string=)
   :selection-callback callback))


(defun wg-button (label callback)
  (make-instance
   'capi:push-button
   :text label
   :callback-type :interface
   :callback callback))


(defun wg-window (title &rest list)
  (capi:contain
   (make-instance
    'capi:column-layout
    :description list)
   :best-x '(- (/ :screen-width 2) 200)
   :best-y '(- (/ :screen-height 2) 100)
   :best-width 550
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
  (let ((dir (capi:prompt-for-directory 
              "Válassza ki a mappát a hibajelzés mentéshez"
              :use-file-dialog t
              :pathname (appdir))))
    (save-forms
     (make-pathname :defaults dir
                    :name (concatenate 'string "error-" (timestamp (get-universal-time)))
                    :type "txt")
     *wg-error-details*))
  (setf *wg-error-details* nil))


(gp:register-image-translation
 'utya-duck
  (gp:read-external-image (concatenate 'string (appdir) "img\\utya-duck.bmp")
;                          :transparent-color-index 7
                          ))


(defun display-utya-duck (pane x y width height)
;  (let ((image (gp:load-image port 'utya-duck)))
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
    :items (list "Hibajelzés mentése" "Kilépés")
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

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
    :visible-min-height '(character 30))
   ;; Progress bar.
   (progress
    capi:progress-bar
    :accessor progress
    :start 0
    :end 100))
  (:default-initargs
   :title "Feldolgozás"
   :best-x 735
   :best-y 400))


(defun init-progress (title buffername)
  (let ((interface (make-instance 'progress :title title :buffer-name buffername)))
    (capi:modify-editor-pane-buffer (text-dump interface) :contents "")
    (capi:display interface)
    interface))


(defmacro with-progress ((title mover dumper count &optional (buffername "temp")) &body body)
  (let ((interface (gensym))
        (i         (gensym)))
  `(let ((,interface (init-progress ,title ,buffername))
         (,i         0))
     (flet ((,mover (&optional n)
              (let ((percent (if (and n (numberp n) (<= n 100))
                               n
                               (progn
                                 (incf ,i)
                                 (round (* 100 (/ ,i ,count)))))))
                (setf (capi:range-slug-start (progress ,interface)) percent)))
            (,dumper (string)
              (let* ((buffer (editor:buffer-from-name ,buffername))
                     (point  (editor:buffers-end buffer)))
                (editor:insert-string point string))))
       ,@body
       (,dumper (format nil "A feldolgozás véget ért, kérem zárja be ezt az ablakot.~%"))))))


;; ----------------------------------------------------------------------
;; Main window


(defun wg-file-selector (label filter filters callback text); accessor)
  (make-instance
   'capi:text-input-pane
   :title label
   :text text
;  :accessor accessor
   :buttons `(:browse-file
              (;:pathname ,(sys:get-folder-path :my-documents)
               :if-does-no-exist :prompt
               :filter ,filter
               :filters ,filters)
              :ok nil)
   :callback callback
;   :callback-type :interface
   :change-callback callback
;   :change-callback-type :interface
   ))


(defun wg-dir-selector (label callback text); accessor)
  (make-instance
   'capi:text-input-pane
   :title label
   :text text
;   :text "Kezdeti érték, mentve az elõzõ menetbõl"
 ;  :accessor accessor
   :buttons `(:browse-file
              (:directory t
;               :pathname ,(sys:get-folder-path :my-documents)
               :if-does-no-exist :prompt
               :use-file-dialog t)
              :ok nil)
   :callback callback
;   :callback-type :interface
   :change-callback callback
;   :change-callback-type :interface
   ))


(defun wg-options (label callback items)
  (make-instance
   'capi:option-pane
   :title label
   :items items
   :selected-item (first items)
   :selection-callback callback
   ))


(defun wg-button (label callback)
  (make-instance
   'capi:push-button-panel
   :items (list label)
   :layout-args '(:x-uniform-size-p t)
   :callback-type :interface
   :callbacks (list callback)))


(defun wg-window (title &rest list)
  (capi:contain
   (make-instance
    'capi:column-layout
    :description list)
   :best-x '(- (/ :screen-width 2) 200)
   :best-y '(- (/ :screen-height 2) 100)
   :title title))


(defun ws-msg (string &rest rest)
  (apply #'capi:display-message string rest))

;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)
(require "shell-objs")
      




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


(defun wg-password-input (callback change-callback text &rest rest)
  (apply #'make-instance
         (append (list 'capi:password-pane
                       :text text
                       :callback callback
                       :change-callback change-callback)
                 rest)))


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
   :text-change-callback callback))


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


(defun wg-window (contain-args &rest list)
  (apply #'capi:contain
   (make-instance
    'capi:grid-layout
    :rows (ceiling (/ (length list) 2))
    :description list)
   (append contain-args
           (list :best-x '(- (/ :screen-width 2) 200)
                 :best-y '(- (/ :screen-height 2) 100)))))


(defun wg-login (title &optional (username nil))
  (block login
    (let* ((username* username)
           (password* nil)
           (username-text-pane (wg-text-input
                                #'(lambda (text &rest rest)
                                    (declare (ignore rest))
                                    (setf username* text))
                                username*))
           (password-text-pane (wg-password-input
                                #'(lambda (text interface)
                                    (setf password* text)
                                    (capi:destroy interface)
                                    (return-from login (values username* password*)))
                                #'(lambda (text &rest rest)
                                    (declare (ignore rest))
                                    (setf password* text))
                                "")))
      (capi:contain
       (make-instance
        'capi:grid-layout
        :rows 3
        :description
        (list " Felhasználónév"
              username-text-pane
              " Jelszó"
              password-text-pane
              (make-instance
               'capi:push-button-panel
               :items '("OK" "Mégsem")
               :selection-callback #'(lambda (data interface)
                                       (capi:destroy interface)
                                       (if (string= data "OK")
                                         (return-from login (values username* password*))
                                         (return-from login (values username* nil)))))))
       :best-x '(- (/ :screen-width 2) 200)
       :best-y '(- (/ :screen-height 2) 100)
       :title title
       :best-width 200
       :max-height 80
       :window-styles '(:tool)
       :initial-focus (if (and username
                               (stringp username)
                               (string/= username ""))
                        password-text-pane
                        username-text-pane)
       :as-dialog :no-escape-button))))


(defun wg-msg (string &rest rest)
  (apply #'capi:display-message string rest))


(defun wg-floating-message (string &optional (timeout 4))
  (capi:display-non-focus-message string :timeout timeout))


(defun wg-confirm (string &rest rest)
  (funcall #'capi:confirm-yes-or-no string rest))





;; ----------------------------------------------------------------------
;; Progress window


(capi:define-interface progress-window ()

  ;; CUSTOM SLOTS =======================================================
  ((full-dump     :accessor full-dump     :initarg   :full-dump)
   (aborted-p     :accessor aborted-p     :initform  nil)
   (progress-pos  :accessor progress-pos  :initform  0)
   (start-time    :accessor start-time    :initform  (get-internal-real-time))
;   (summary-start :accessor summary-start :initform  nil)
   (buffer-name   :accessor buffer-name   :initarg   :buffer-name)
   (text-stream   :accessor text-stream   :initarg   :text-stream)
   (title         :accessor title         :initarg   :title)
   (step-count    :accessor step-count    :initarg   :step-count)
   (exit-tag      :accessor exit-tag      :initarg   :exit-tag))

  (:panes ; =============================================================
   (text-disp ; Text output by working thread. --------------------------
    capi:collector-pane
    :accessor           text-disp
    :flag               'minimal-example
    :buffer-name        buffer-name
    :enabled            :read-only
    :visible-min-width  '(character 80)
    :visible-min-height '(character 30)
    :wrap-style         :split-on-space
    :vertical-scroll    t)
   
   (progress-bar ; Progress bar -----------------------------------------
    capi:progress-bar
    :accessor progress-bar
    :start 0
    :end 100)

   (rest-time ; Remaining time display ----------------------------------
    capi:title-pane
    :accessor rest-time
    :text "")


   ;; 'scroll to bottom' button

   
   (abort-button ; "Abort" button ---------------------------------------
    capi:push-button
    :accessor abort-button
    :text "Megszakítás"
    :callback-type :interface
    :callback (:initarg abort-callback))

   (send-details-button ; "Send error details" button -------------------
    capi:push-button
    :accessor messages-button
    :text "Hibajelzés küldése/mentése"
    :callback-type :interface
    :callback (:initarg send-details-callback)
    :enabled nil)

   (close-button ; "Close window" button --------------------------------
    capi:push-button
    :accessor close-button
    :text "Bezárás"
    :callback-type :interface
    :callback (:initarg close-callback)
    :enabled nil))

  (:layouts ; ===========================================================
   (upper  capi:column-layout '(text-disp progress-bar rest-time))
   (lower  capi:row-layout '(abort-button send-details-button close-button))
   (window capi:column-layout '(upper lower)))


  (:default-initargs ; ==================================================
   :title  "Feldolgozás"
   :best-x 735
   :best-y 400
   :layout 'window
   :buffer-name (random-alphanumeric-string 6)
   :exit-tag nil
   
   :confirm-destroy-function #'(lambda (interface)
                                 (capi:button-enabled (close-button interface)))

   :abort-callback #'(lambda (interface)
                       (when (and (not (aborted-p interface))
                                  (wg-confirm "Megszakítja a feldolgozást?"))
                         (wg-floating-message "Megszakítás ..." 3)
                         (setf (aborted-p interface) t)))

   :create-callback #'(lambda (interface)
                        (setf (text-stream interface)
                              (capi:collector-pane-stream (text-disp interface)))
                        (capi:modify-editor-pane-buffer (text-disp interface) :contents ""))))


(defmethod progress-send-details-callback-fn ((obj wax-app))
  #'(lambda (interface)
      (setf (full-dump interface) (full-dump obj))
      (wg-send-error interface)))
      

(defmethod progress-close-callback-fn ((obj wax-app))
  #'(lambda (interface)
      (purge-errorlogs obj)
      (purge-errordumps obj)
      (capi:destroy interface)))


(defun progress-time-string (secs)
  (let* ((hours (truncate (/ secs 3600)))
         (rem1  (- secs (* hours 3600)))
         (mins  (truncate (/ rem1 60)))
         (secs  (- rem1 (* mins 60)))
         (accum '()))
    (unless (zerop hours)
      (push (format nil "~d:" hours) accum))
    (push (format nil "~2,'0d:~2,'0d" mins secs) accum)
    (apply #'concatenate 'string (nreverse accum))))


(defmethod switch-buttons ((interface progress-window) errorlogs-pending-p)
  (setf (capi:button-enabled (close-button interface)) t
        (capi:button-enabled (abort-button interface)) nil)
  (when errorlogs-pending-p
    (setf (capi:button-enabled (messages-button interface)) t)))


(defmethod step-progress ((interface progress-window) &key (step-count nil) (abs nil) (step 1))
  (let* ((percent (if (and abs (numberp abs) (<= abs 100))
                    abs
                    (* 100 (/ (incf (progress-pos interface) step)
                              (or step-count (step-count interface))))))
         (current-time (get-internal-real-time))
         (time-spent   (/ (- current-time (start-time interface))
                          internal-time-units-per-second))
         (time-left    (max (- (* time-spent (/ 100 percent)) time-spent)
                            0)))
    (setf (capi:range-slug-start (progress-bar interface)) (round percent)
          (capi:title-pane-text  (rest-time interface))
          (format nil "Eltelt idõ: ~a,  becsült hátralévõ idõ: ~a"
                  (progress-time-string (round time-spent))
                  (progress-time-string (round time-left))))))


(defmethod abort-progress-when-requested ((interface progress-window))
  (when (aborted-p interface)
    (format (text-stream interface) "~%~%A feldolgozás megszakítva.~%")
    (switch-buttons interface nil)
    (with-slots (exit-tag) interface
      (when exit-tag
        (throw exit-tag nil)))))


(defmethod progress ((interface progress-window) &key (step-count nil) (abs nil) (step 1))
  (step-progress interface :step-count step-count :abs abs :step step)
  (abort-progress-when-requested interface))


(defmethod kill-progress-fn ((interface progress-window))
  #'(lambda (errorlogs-pending-p)
      (setf (aborted-p interface) t)
      (format (text-stream interface) "~%~%A feldolgozás félbeszakadt.~%")
      (switch-buttons interface errorlogs-pending-p)))


(defmethod wrap-up-progress ((interface progress-window) (obj wax-app))
  (with-slots (text-disp text-stream) interface
    (let ((summary-start nil)
          (line (line 70 #\*)))
      (when (errorlogs-pending-p obj)
        (setf summary-start (capi:get-vertical-scroll-parameters text-disp :max-range))
        (format text-stream "~3%~a~%HIBÁK RÉSZLETEZÉSE:~%~a~2%" line line)
        (display-errorlogs obj text-stream))
      (switch-buttons interface (errorlogs-pending-p obj))
      (format text-stream "~2%A feldolgozás befejezõdött.~%")
      (when summary-start
        (capi:execute-with-interface interface
          #'(lambda () (capi:scroll text-disp :vertical :move summary-start)))))))


(defmacro with-progress-window ((interface-var step-count wax-app
                                               &key (title "Feldolgozás")
                                               (stream nil)) &body body)
  `(let ((,interface-var (make-instance 'progress-window :step-count ,step-count
                                        :title ,title :exit-tag (gensym)
                                        :close-callback (progress-close-callback-fn ,wax-app)
                                        :send-details-callback (progress-send-details-callback-fn
                                                                ,wax-app))))
     (setf (kill-fn ,wax-app) (kill-progress-fn ,interface-var))
     (catch (exit-tag ,interface-var)
       (progn
         (with-slots ((,(or stream 's) text-stream)) ,interface-var
           (capi:display ,interface-var)
           ,@body
           (wrap-up-progress ,interface-var ,wax-app))))))


(defun p3tb ()
  (let ((obj (make-instance 'wax-app)))
    (with-wax-errorsink (obj :enabled t)
      (with-progress-window (interface 100 obj :title "Progress teszt" :stream s)
        (dotimes (i 100)
          (sleep 0.2)
          (format s "vazz, ~a~%" i)
          (when (zerop (mod i 3))
            (queue-errorlog obj (format nil "~a osztható 3-mal!~%" i)))
;          (when (= i 29)
;            (format s "0 / 0 = ~a~%" (/ 0 0)))
          (progress interface))))))





;; ----------------------------------------------------------------------
;; Error dialog


(defun dumpfile-name (&key dir (use-tempdir nil))
  (let* ((name  (concatenate 'string (string-downcase (package-name *package*))
                             "-error-" (timestamp (get-universal-time))))
         (dir*  (cond (dir         (list :defaults dir))
                      (use-tempdir (list :defaults (hcl:get-temp-directory)))
                      (t           nil)))
         (pname (apply #'make-pathname
                          (append (list :name name :type "txt") dir*))))
    (namestring pname)))


(defun wg-save-error (interface)
  (let ((file (capi:prompt-for-file
               "Válassza ki a hibajelzés mentésének helyét!"
               :pathname (dumpfile-name :dir (appdir))
               :filter "*.txt"
               :filters '("Szövegfájlok" "*.txt"
                          "Minden fájl" "*.*")
               :if-exists :prompt
               :if-does-not-exist :ok
               :operation :save)))
    (when file
      (save-forms file (full-dump interface))
      (wg-msg "A hibajelzés elmentve:~%~a" file))
    (capi:destroy interface)))


(defun wg-send-error (interface)
  (let ((dumpfile (dumpfile-name :use-tempdir t)))
    (if (outlook-running-p)
      (progn
        (save-forms dumpfile (full-dump interface))
        (new-mail "denes.cselovszki@kk.gov.hu"
                  (format nil "~a hibajelzés" (package-name *package*))
                  :attch dumpfile)
        (capi:destroy interface))
      (progn
        (wg-save-error interface)))))


(gp:register-image-translation
 'utya-duck
  (gp:read-external-image (concatenate 'string (appdir) "img\\utya-duck.bmp")))


(defun display-utya-duck (pane x y width height)
  (declare (ignore x y width height))
  (let ((image (gp:load-image pane 'utya-duck)))
    (gp:draw-image pane image 0 0)))


(capi:define-interface errordial ()
  ((full-dump :accessor full-dump :initarg :full-dump))
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

   (messages-button
    capi:push-button
    :accessor messages-button
    :text "Hibajelzés küldése/mentése"
    :callback-type :interface
    :callback #'wg-send-error)
   (close-button
    capi:push-button
    :accessor close-button
    :text "Bezárás"
    :callback-type :interface
    :callback #'capi:quit-interface)
   )
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
    '(messages-button close-button)
;    '(two-buttons)
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
   :full-dump ""
   ))


(defun wg-errordial (obj) ;message details)
;  (setf *wg-error-details* (append (list :message message) details))
  (capi:contain
   (make-instance
    'errordial
    :message (first (errorlogs obj))
    :full-dump (full-dump obj))))

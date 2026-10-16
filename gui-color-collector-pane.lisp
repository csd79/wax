;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:wax)
      

#|
COLOR-COLLECTOR-PANE is a subclass of CAPI:COLLECTOR-PANE. It allows basic
text formating via escape sequences inserted into the printing text.
We can change the color, font, size and styling, and can also insert
clickable links with callbacks.

COLOR-COLLECTOR-PANE consumes the same as COLLECTOR-PANE.
Its input stream can be acquired using (CAPI:COLLECTOR-PANE-STREAM pane).
We can print text into the stream using the FORMAT function.

Formating directives can be inserted using the $[...] escape sequence.
Directives can describe concrete formating, but also we can choose from
pre-defined stylings stored in the *STYLES* global variable.

  "This is a $[bold,bg=:red]word$[clr] with a different styling."

Style directives can be constructed by interpolating FORMAT arguments.

Supported directives:

fg, bg       Foreground and background color.
             Valid values: CAPI color name keywords, or hex RGB triplets.
             Examples: 'fg=:red'
                       'bg=#1188bb'  ; digits are case insensitive.

font         Font family. Example: 'font=Times New Roman'

size         Font size. Example: 'size=12'

bold, it,    Bold, italic or underlined.
uline        Example: 'bold,uline'

style        Selects a predefined styling.
             Overrides any other directives in the same escape sequence
             other than 'clr', see below.
             Example: 'style=:alert'
             The default style is 'normal':

link         The pane instance contains an associative list
             of symbols and callback functions (of zero parameter).
             This list can be populated using the ADD-LINK method.
             Using the 'link' directive, we can bind any stored
             callback function to a text snippet by adding the name
             of the associated symbol.
             Example: 'link=G2214'

clr          Switches to the default styling (:normal).
             Its purpose is to switch off the previous formating.
             Overrides any other directives in the same escape sequence,
             including 'style'.

ADD-LINK can be used to create links right in the argument list of FORMAT:

  (format s "Text. $[link=~a]Link 1$[clr]. Text. $[link=~a]Link 2$[clr]."
          (add-link pane #'first-callback)
          (add-link pane #'second-callback))

ADD-LINK creates a fresh symbol and uses it as a key to store the
callback function within the pane instance. ADD-LINK also returns the name
of the symbol, which has to be inserted into the $[link=...] directive.
The symbol is then stored as a text property for the link by the custom
write function of COLOR-COLLECTOR-PANE's underlying mechanism. Upon
cliking the link, the stored symbol will be retrieved, and the associated
callback function called.

Link callbacks must be functions of zero parameters.
|#





;; ----------------------------------------------------------------------
;; Predefined stylings


(defun defstyle (&key (fg :black) (bg nil) (font "Courier") (size 10)
                     (bold nil) (it nil) (uline nil) &allow-other-keys)
  "Prepare face definition parsable by EDITOR:MAKE-FACE."
  (nconc (when fg    `(:foreground  ,fg))
         (when bg    `(:background  ,bg))
         (when bold  `(:bold-p      ,bold))
         (when it    `(:italic-p    ,it))
         (when uline `(:underline-p ,uline))
         `(:font ,(gp:make-font-description
                   :family font
                   :size   size
                   :weight (if bold :bold :normal)
                   :slant  (if it :italic :roman)))))


(defparameter *styles*
  `(:normal    ,(defstyle)
    :link      ,(defstyle :fg :blue :uline t)
    :bold      ,(defstyle :bold t)
    :bold-ul   ,(defstyle :bold t :uline t)
    :italic    ,(defstyle :it t)
    :blue      ,(defstyle :fg :blue)
    :blue-bold ,(defstyle :fg :blue :bold t)
    :red       ,(defstyle :fg :red)
    :red-bold  ,(defstyle :fg :red :bold t)
    :alert     ,(defstyle :fg :red :bg :yellow :bold t)
    ))





;; ----------------------------------------------------------------------
;; Command -> face


(defun hexrgb-p (value)
  "Is VALUE a string containing a hex color descriptor (in the form of '#1188ff')?"
  (and (stringp value)
       (= (length value) 7)
       (char= (char value 0) #\#)
       (every #'(lambda (char)
                  (let ((c (char-code char)))
                    (or (<= 48 c 57)
                        (<= 65 c 70)
                        (<= 97 c 102))))
              (subseq value 1))))


(defun hexrgb->color (string)
  "Turn a textual hex color descriptor into a CAPI color."
  (flet ((hex->dec (start end)
           (ignore-errors (parse-integer string :start start :end end :radix 16))))
    (apply #'color:make-rgb
           (mapcar #'(lambda (start end)
                       (/ (hex->dec start end) 255.0))
                   '(1 3 5) '(3 5 7)))))


(defun subcommand-pair (list)
  "Turn the first two elements of LIST into a key/value pair."
  (destructuring-bind (key val &rest rest) list
    (declare (ignore rest))
    (list (string->keyword key)
          (or (ignore-errors (parse-integer val))       ; VAL might be an integer,
              (when (and (stringp val)                  ; ... or a keyword,
                         (char= (char val 0) #\:))
                (string->keyword (subseq val 1)))
              (when (hexrgb-p val) (hexrgb->color val)) ; ... or a hex color descriptor,
              val))))                                   ; ... or a string.


(defun destructure-face-command (string)
  "Turn a face command into a plist that DEFSTYLE can understand."
  (apply #'nconc
         (mapcar #'(lambda (sub)
                     (funcall #'subcommand-pair
                              (if (position #\= sub)
                                (str:split #\= sub)
                                (list sub t))))
                 (str:split #\, string))))





;; ----------------------------------------------------------------------
;; Writer


;;; COLLECTOR-PANE subclass with support for displaying colored texts.
(defclass color-collector-pane (capi:collector-pane)
  ((active-face        :initform nil :accessor active-face)
   (active-link        :initform nil :accessor active-link)
   (system-stream      :initform nil :accessor system-stream)
   (interceptor-stream :initform nil :accessor interceptor-stream)
   (links              :initform '() :accessor links)))


(defun current-b1press (pane)
  "Grab the current callback function for mouse left click on PANE."
  (let* ((input-model (capi:output-pane-input-model pane))
         (b1press     (remove-if-not #'(lambda (spec)
                                         (equal (first spec) '(:button-1 :press)))
                                     input-model)))
    (destructuring-bind ((gesture function argument)) b1press
      (declare (ignore gesture))
      (values function argument))))


(defmethod add-link ((pane color-collector-pane) function &optional (label nil))
  "Save a link callback fn, generate a label for it and return that label."
  (let ((symbol (intern (symbol-name (gensym)))))
    (push (cons symbol function) (links pane))
    (symbol-name symbol)))


(defmethod get-link-callback ((pane color-collector-pane) label)
  "Return the callback function saved for LABEL."
  (cdr (assoc label (links pane))))


#|
  This function was supposed to be part of COLOR-COLLECTOR-PANE's input model,
  modifying the mouse cursor when hovering over a text link. AFAIK there is
  no public way in LW Editor to do this. EDITOR::WINDOW-POINT-FROM-MOUSE-
  POSITION seems to be the needed function, but I am not sure about its
  parameters:
  - win: not a CAPI window but an internal structure name window, see
    'struct2.lisp' in the Editor source.
  - py, char-y: how should I know these?
  Gemini went in circles for a whiel trying to figure this out.
  |#
#|(defun update-mouse-cursor-type (pane x y)
  (capi:apply-in-pane-process pane
    (lambda ()
      (let* ((buffer (capi:editor-pane-buffer pane))
             (win    (editor:current-window)))
        (when (and buffer win)
          ;; CRITICAL: Bind the structural layout synchronization block 
          ;; so window-point-from-mouse-position runs safely.
          (editor:with-buffer-locked (buffer)
            (let ((point (editor::window-point-from-mouse-position win 0 0 x y)))
              (let ((target-cursor :i-beam))
                (when point
                  ;; Scan the character block for your callback property metadata
                  (let ((link (editor:get-text-property point :link)))
                    (when link
                      (setf target-cursor :hand))))
                ;; Update the mouse cursor icon seamlessly on the main CAPI window thread
                (capi:execute-with-interface (capi:element-interface pane)
                  (lambda ()
                    (unless (eq (capi:simple-pane-cursor pane) target-cursor)
                      (setf (capi:simple-pane-cursor pane) target-cursor))))))))))))|#


(defmethod initialize-instance :after ((pane color-collector-pane) &key)
  "After initializing our COLOR-COLLECTOR-PANE instance, add callbacks to textual links."
  (multiple-value-bind (func arg)
      (current-b1press pane)
    (setf (capi:output-pane-input-model pane)
          (list* `((:button-1 :press)
                   ,#'(lambda (pane x y &rest rest)
                        (declare (ignore rest))
                        (funcall (symbol-function func) pane x y arg)
                        (let* ((buffer   (capi:editor-pane-buffer pane))
                               (point    (editor:buffer-point buffer))
                               (callback (get-link-callback pane
                                           (editor:get-text-property point :link))))
                          (when callback (funcall callback)))))
#|                   `((:motion)
                   ,#'(lambda (pane x y &rest rest)
                        (declare (ignore rest))
                        (update-mouse-cursor-type pane x y)))|#
                 (capi:output-pane-input-model pane)))))
  

(defmethod switch-face ((pane color-collector-pane) &optional (command-text nil))
  "Create face for PANE defined by COMMAND, to be used by HCL:WRITE-STRING-WITH-PROPERTIES."
  (let* ((command (when command-text (destructure-face-command command-text)))
         (actual  (cond ((null command-text) (getf *styles* :normal))
                        ((member :clr command)
                         (getf *styles* :normal))
                        ((member :link command)
                         (getf *styles* :link))
                        ((member :style command)
                         (or (getf *styles* (getf command :style))
                             (getf *styles* :normal)))
                        (t (apply #'defstyle (remove-pairs command '(link)))))))
    (setf (active-face pane) (apply #'editor:make-face
                                    nil
                                    :font (gp:find-best-font pane (getf actual :font))
                                    (remove-pairs actual '(:font)))
          (active-link pane) (when (getf command :link)
                               (find-symbol (getf command :link))))))


(declaim (inline cmd-start cmd-end))
  (defun cmd-start (string start)
    "Search for starting point of style directive."
    (search "$[" string :start2 start))

  (defun cmd-end (string cmd-start)
    "Search for ending point of style directive."
    (when cmd-start (position #\] string :start (+ cmd-start 2))))


(defun 1st-chunk-end (string start)
  "What is the last position before the next style directive?"
  (let* ((cmd-start    (cmd-start string start))
         (cmd-end      (cmd-end string cmd-start))
         (endedp       (numberp cmd-end)))
    (if endedp cmd-start (length string))))


(defun next-command (string start)
  "What is the next style directive?"
  (let* ((cmd-start    (cmd-start string start))
         (cmd-end      (cmd-end string cmd-start))
         (startedp     (numberp cmd-start))
         (endedp       (numberp cmd-end)))
    (and startedp endedp (subseq string (+ cmd-start 2) cmd-end))))


(defun next-chunk-start (string start)
  "What is the first position after the next style directive?"
  (let* ((cmd-start    (cmd-start string start))
         (cmd-end      (cmd-end string cmd-start))
         (endedp       (numberp cmd-end)))
    (when endedp (1+ cmd-end))))


(defmethod editor-current-end ((pane color-collector-pane))
  "Ending point of PANE's editor buffer."
  (let ((buffer (capi:editor-pane-buffer pane)))
    (editor:copy-point (editor:buffers-end buffer))))


(defun custom-write (pane string stream)
  "Write STRING with properties parsed from it."
  (let ((interface (capi:element-interface pane)))
    (when interface
      (switch-face pane)
      (capi:execute-with-interface interface
        #'(lambda ()
            (loop with i = 0
                  for 1st-chunk-end    = (1st-chunk-end string i)
                  for next-command     = (next-command string i)
                  for next-chunk-start = (next-chunk-start string i)
                  while (< i (length string)) do
                  (unless (zerop 1st-chunk-end)
                    (let ((highlight `(:highlight ,(active-face pane)))
                          (link      (when (active-link pane)
                                       `(:link ,(active-link pane)))))
                      (hcl:write-string-with-properties
                       string
                       (append highlight link)                                 
                       stream :start i :end 1st-chunk-end)))
                  (setf i (or next-chunk-start 1st-chunk-end))
                  (when next-command (switch-face pane next-command))))))))


;; ----------------------------------------------------------------------
;; Custom collector-pane & stream


;;; A separate class of stream that we can hook our custom writing mechanism on.
(defclass custom-stream (stream:fundamental-character-output-stream)
  ((target-pane :initarg  :pane   :reader   target-pane)
   (dest-stream :initarg  :actual :reader   dest-stream)
   (flush-timer :initform nil     :accessor flush-timer)
   (stream-lock :initform (mp:make-lock :name "Color Stream Lock")
                :reader   stream-lock)
   (line-buffer :initform (make-array 256 :element-type 'character :adjustable t :fill-pointer 0)
                :reader   line-buffer)))


(defun schedule-auto-flush (stream)
  (mp:with-lock ((stream-lock stream))
    ;; Initialize the timer instance if it doesn't exist yet
    (unless (flush-timer stream)
      (setf (flush-timer stream)
            (mp:make-timer 
             #'(lambda ()
                 ;; Execute STREAM-FINISH-OUTPUT in the interface thread.
                 (let ((interface (capi:element-interface (target-pane stream))))
                   (when interface
                     (capi:execute-with-interface interface
                       #'(lambda () (stream:stream-finish-output stream)))))))))
    ;; Schedule stream flush.
    (mp:schedule-timer-relative (flush-timer stream) 0.02)))


(defmethod stream:stream-write-char ((stream custom-stream) char)
  "Write character to CUSTOM-STREAM using the colorful writing function."
  (mp:with-lock ((stream-lock stream))
    (vector-push-extend char (line-buffer stream)))
  (schedule-auto-flush stream)                      ;
  char)


(defmethod stream:stream-write-string ((stream custom-stream) string
                                       &optional (start 0) (end (length string)))
  "Write string to CUSTOM-STREAM using the colorful writing function."
  (let ((substring (subseq string start end)))
    (mp:with-lock ((stream-lock stream))
      (loop for c across substring doing
            (vector-push-extend c (line-buffer stream))))
    (schedule-auto-flush stream)
    substring))


(defmethod stream:stream-finish-output ((stream custom-stream))
  "Push buffered output for STREAM out the door."
  (let ((copied-text nil))
    ;; Copy current contents of LINE-BUFFER.
    (mp:with-lock ((stream-lock stream))
      (let ((buffer (line-buffer stream)))
        (when (> (length buffer) 0)
          (setf copied-text (copy-seq buffer))
          (setf (fill-pointer buffer) 0))))
    ;; Display copied text.
    (when copied-text
      (custom-write (target-pane stream) copied-text (dest-stream stream))))
  nil)


(defmethod stream:stream-force-output ((stream custom-stream))
  "Same as above."
  (stream:stream-finish-output stream))


(defmethod capi:collector-pane-stream ((pane color-collector-pane))
  "Add the customized stream to out COLOR-COLLECTOR-PANE instance if it hasn't been added yet, then return it."
  (or (interceptor-stream pane)
      (let ((system-stream (call-next-method)))
        (setf (system-stream pane) system-stream
              (interceptor-stream pane) (make-instance 'custom-stream
                                                       :pane   pane
                                                       :actual system-stream)))))





;; ----------------------------------------------------------------------
;; Sandbox


(capi:define-interface stream-format-window ()
  ()
  (:panes   (text-area color-collector-pane :vertical-scroll t :accessor text-area))
  (:layouts (main-layout capi:column-layout '(text-area)))
  (:default-initargs :title "Test!" :width 600 :height 350))


(defun run-format-stream-demo ()
  (let* ((interface (capi:display (make-instance 'stream-format-window)))
         (pane      (slot-value interface 'text-area))
         (stream    (capi:collector-pane-stream pane)))
    (format stream "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum gravida sem eu egestas tristique. Donec rutrum quis arcu porta pellentesque. Fusce a lorem consectetur, pellentesque turpis vitae, convallis arcu. Praesent vitae gravida velit. Aenean tortor lorem, $[bold]elementum$[clr] sed $[link=~a]bibendum$[clr] convallis, dignissim ac nulla. Nunc nec odio fermentum arcu porta condimentum in eget mauris. Ut $[font=~a,it,bold]sollicitudin$[clr] volutpat velit. Praesent quam arcu, eleifend id odio ac, faucibus imperdiet risus. Vestibulum nec accumsan elit, eu mollis justo. Nulla sit amet ipsum a urna cursus posuere mollis vel augue. Praesent egestas accumsan augue, eget dictum neque. Proin a tellus dolor. Aliquam sagittis consequat magna, vel $[link=~a]commodo$[clr] dolor $[bg=#220044,fg=#FFFFFF,size=14]ullamcorper$[clr] non.~%"
            (add-link pane #'(lambda () (wg-msg "MICIMACKÓÓÓ!!!")))
            "Times New Roman"
            (add-link pane #'(lambda () (wg-msg "MALACZKAAAAAAA??"))))))


(defun run-multithread-stress-test ()
  (let* ((interface (capi:display (make-instance 'stream-format-window)))
         (stream    (capi:collector-pane-stream (slot-value interface 'text-area))))
    (format stream "Main system process spawning worker threads...~%")
    (mp:process-run-function "Worker-Thread-A" '()
      #'(lambda ()
          (loop for i from 1 to 3
                do (sleep 0.8)
                   (format stream "[Thread A]: Step $[bold,fg=:blue]~D/3$[clr] -> Parsing binary manifest... $[fg=:green]OK$[clr].~%" i))))
        (mp:process-run-function "Worker-Thread-B" '()
      #'(lambda ()
          (loop for j from 1 to 3
                do (sleep 1.2)
                   (format stream "[Thread B]: Alert sequence $[fg=#FF5500,bold]#0~D$[clr] running. Core temp: $[bg=#220044,fg=#FFFFFF,size=14] STABLE $[clr].~%" j))))
    (mp:process-run-function "Monitor-Thread" '()
      #'(lambda ()
          (sleep 4.0)
          (format stream "$[style=:alert]=== ALL WORKER PROCESSES CLOSED SAFELY ===$[clr]~%")))))

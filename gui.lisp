(in-package #:claret-gui)

(declaim (optimize (debug 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The info pane and how to display it.

(defclass claret-info-pane (info-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20
   :display-function 'display-info))

;;; right now, we only display the name of the buffer that is on
;;; display in the master pane.  It would be a good idea to have named
;;; views, and display the name of the buffer AND the name of the view.
(defun display-info (frame pane)
  (format pane "   ~a   ~a"
	  (name (buffer (stream-default-view (master-pane pane))))
	  ;; display the string "Def" if we are currently
	  ;; recording a keyboard macro.
	  (if (recordingp frame) "Def" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The minibuffer pane

(defclass claret-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The application panes

(defclass claret-pane (esa-pane-mixin application-pane) ())

;;; We define a special version of the vbox pane that contains only
;;; an application pane and an info pane.
(defclass pair-pane (vbox-pane)
  ((%main-pane :initarg :main-pane :reader main-pane)
   (%info-pane :initarg :info-pane :reader info-pane)))

(defmethod view ((pane pair-pane))
  (stream-default-view (main-pane pane)))

(defmethod (setf view) (new-view (pane pair-pane))
  (setf (stream-default-view (main-pane pane)) new-view))

(defun make-pair-pane (width height)
  (let* ((main-pane (make-pane 'claret-pane
			       :width width :height height
			       :display-function 'display-claret-pane))
	 (info-pane (make-pane 'claret-info-pane
			       :master-pane main-pane
			       :width width)))
    (make-pane 'pair-pane
	       :main-pane main-pane
	       :info-pane info-pane
	       :contents (list (scrolling () main-pane )
			       info-pane))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Views

(defclass claret-view (view)
  ((%buffer :initarg :buffer :reader buffer)
   (%cursor :initarg :cursor :accessor view-cursor)))

(defmethod initialize-instance :after ((view claret-view)
				       &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (view-cursor view)
	(graph:make-initial-cursor (buffer view))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The application frame

(define-application-frame claret-frame (esa-frame-mixin
					 standard-application-frame)
  ((%views :initform '() :initarg :views :accessor views))
  (:panes
   (window (let* ((pair-pane (make-pair-pane 900 800))
		  (main-pane (main-pane pair-pane)))
	     ;; ESA needs a list of windows in the application frame
	     ;; when the top-level is run.
	     (setf (windows *application-frame*)
		   (list main-pane))
	     pair-pane))
   (minibuffer (make-pane 'claret-minibuffer-pane :width 900)))
  (:layouts
   (default
       (vertically (:scroll-bars nil)
	 window
	 minibuffer)))
  (:top-level (esa-top-level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; main entry points

(defun claret (&rest args &key new-process process-name width height)
  "Start a Claret session with a fresh empty buffer"
  (declare (ignore new-process process-name width height))
  (apply #'claret-common '(com-new-buffer) args))

(defun edit-file (filename &rest args
                  &key new-process process-name width height)
  "Start a Claret session editing a given file"
  (declare (ignore new-process process-name width height))
  (apply #'claret-common `(esa-io::com-find-file ,filename) args))

(defun claret-common (command &key new-process (process-name "Claret")
			      (width 900) (height 800))
  (let* ((*application-frame*
	  (make-application-frame 'claret-frame :width width :height height))
	 (*esa-instance* *application-frame*)
	 (application-frame *application-frame*))
    (adopt-frame (find-frame-manager) application-frame)
    (execute-frame-command application-frame command)
    (flet ((run () (run-frame-top-level application-frame)))
      (if new-process
          (clim-sys:make-process #'run :name process-name)
          (run)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying panes

;;; trampoline from the display function of the
;;; application pane, to this function that includes
;;; the view of the pane to display.  This is how we
;;; get access to the buffer.
(defgeneric display-frame-pane-view (frame pane view))

(defun display-claret-pane (frame pane)
  (display-frame-pane-view frame pane (stream-default-view pane)))

(defgeneric display-component (pane component))

(defmethod display-frame-pane-view ((frame claret-frame)
				    (pane claret-pane)
				    (view claret-view))
  (declare (ignore frame))
  (display-component pane (buffer view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command tables

(defun current-view ()
  (stream-default-view (current-window)))

(defgeneric find-claret-command-table (view component))

;;; tell ESA how to find a command table
(defmethod find-applicable-command-table ((frame claret-frame))
  (find-claret-command-table
   (current-view) (graph:leaf (view-cursor (current-view)))))

(define-command-table global-claret-table
    :inherit-from (global-esa-table keyboard-macro-table esa-io-table))

(defmethod find-claret-command-table (view component)
  (declare (ignore view))
  (find-command-table 'global-claret-table))

(define-command-table up-command-table)
(define-command-table down-command-table)
(define-command-table forward-backward-command-table)
(define-command-table delete-child-command-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands and key bindings

(define-command (com-move-up :command-table up-command-table)
    ()
  (graph:move-up (view-cursor (current-view))))

(set-key 'com-move-up 'up-command-table '((#\u :control :meta)))

(define-command (com-move-down :command-table down-command-table)
    ()
  (let ((cursor (view-cursor (current-view))))
    (cond ((graph:at-end-p cursor)
	   (display-message "At end"))
	  (t (graph:move-down cursor 0)))))

(set-key 'com-move-down 'down-command-table '((#\d :control :meta)))

(define-command (com-move-forward :command-table forward-backward-command-table)
    ()
  (let ((cursor (view-cursor (current-view))))
    (cond ((graph:at-end-p cursor)
	   (display-message "At end"))
	  (t (graph:move-forward cursor)))))

(set-key 'com-move-forward 'forward-backward-command-table '((#\f :control)))

(define-command (com-move-backward :command-table forward-backward-command-table)
    ()
  (let ((cursor (view-cursor (current-view))))
    (cond ((graph:at-beginning-p cursor)
	   (display-message "At beginning"))
	  (t (graph:move-backward cursor)))))

(set-key 'com-move-backward 'forward-backward-command-table '((#\b :control)))

(define-command (com-delete-child :command-table delete-child-command-table) ()
  (let ((cursor (view-cursor (current-view))))
    (cond ((graph:at-end-p cursor)
	   (display-message "At end"))
	  (t (graph:delete-child cursor)))))

(set-key 'com-delete-child 'delete-child-command-table '((#\d :control)))

(define-command (com-new-buffer :name t) ()
  (let* ((buffer (make-instance 'document))
	 (view (make-instance 'claret-view :buffer buffer)))
    ;; make the new view the current one
    (push view (views *application-frame*))
    ;; put it on display
    (setf (stream-default-view (current-window)) view)))

(defmethod display-frame-pane-view ((frame claret-frame)
				    pane
				    (view claret-view))
  (declare (ignore frame))
  (display-component pane (buffer view)))

(define-command (com-export-as-postscript :name t :command-table global-claret-table)
    ()
  (with-open-file (stream "stuff.ps" :direction :output :if-exists :supersede)
    (with-output-to-postscript-stream (psstream stream)
      (setf (stream-default-view psstream)
	    (make-instance 'claret-view
	      :buffer (buffer (stream-default-view (current-window)))))
      (display-claret-pane *application-frame* psstream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input/Output

;;; the ESA library needs this information in order to return an
;;; existing buffer when an attempt is made to find a file
;;; that is already present in some buffer of the application.
(defmethod buffers ((frame claret-frame))
  (remove-duplicates (mapcar #'buffer (views frame)) :test #'eq))

;;; tell the ESA library how to find the current buffer of the
;;; application frame.
(defmethod frame-current-buffer ((frame claret-frame))
  (buffer (stream-default-view (current-window))))

;;; the ESA library calls this function to save the contents of
;;; an application buffer to a stream.  We need to supply a method
;;; for it.
(defmethod frame-save-buffer-to-stream
    ((fram claret-frame) (buffer document) stream)
  (write-document-to-stream buffer stream))

;;; tell the ESA library how to create a new empty buffer.
;;; The library calls this function whenever the user attempts
;;; to find a file that doesn't exist.
(defmethod frame-make-new-buffer ((frame claret-frame) &key &allow-other-keys)
  (make-instance 'document))

;;; tell the ESA library how to create a buffer from a stream.
;;; The library calls this function whenever the user has
;;; executed a find-file command with a filename that exists.
(defmethod frame-make-buffer-from-stream ((frame claret-frame) stream)
  (read-document-from-stream stream))

;;; when finding a file, the ESA library calls frame-find-file, which
;;; returns a buffer.  But the ESA library doesn't know what to do
;;; about that buffer.  The application must therefore put an :around
;;; method on that function that stores the buffer in the application
;;; frame.  In the case of this claret application, we also need to
;;; create a view for it.
(defmethod frame-find-file :around ((frame claret-frame) filepath)
  (declare (ignore filepath))
  (let* ((buffer (call-next-method))
	 (view (make-instance 'claret-view :buffer buffer)))
    ;; make the new view the current one
    (push view (views frame))
    ;; put it on display
    (setf (stream-default-view (current-window)) view)))

;;; this method should probably be moved to to the ESA library itself
;;; in the form of two functions that client code can call, one
;;; to ask the user for buffers to save, and another that determines
;;; whether it is safe to exit.
(defmethod frame-exit :around ((frame claret-frame))
  (loop for buffer in (buffers frame)
	when (and (needs-saving buffer)
		  (filepath buffer)
		  (handler-case (accept 'boolean
					:prompt (format nil "Save buffer: ~a ?"
							(name buffer)))
		    (error () (progn (beep)
				     (display-message "Invalid answer")
				     (return-from frame-exit nil)))))
	  do (save-buffer buffer))
  (when (or (notany #'(lambda (buffer) (and (needs-saving buffer) (filepath buffer)))
		    (buffers frame))
	    (handler-case (accept 'boolean :prompt "Modified buffers exist.  Quit anyway?")
	      (error () (progn (beep)
			       (display-message "Invalid answer")
			       (return-from frame-exit nil)))))
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying boxes

(defgeneric display-box (box pane x y))

(defmethod display-box ((box claret-boxes:box) pane x y)
  (declare (ignore pane x y))
  nil)

(defmethod display-box ((box claret-boxes:hbox) pane x y)
  (loop for child in (claret-boxes:children box)
	do (display-box child pane x y)
	do (incf x (claret-boxes:width child))))

(defmethod display-box ((box claret-boxes:vbox) pane x y)
  (loop for child in (claret-boxes:children box)
	do (display-box child pane x y)
	do (incf y (+ (claret-boxes:ascent child) (claret-boxes:descent child)))))

(in-package #:claret-word)

(declaim (optimize (debug 3)))

(defclass word (graph:component graph:with-children-mixin)
  ()
  (:default-initargs :children ""))

;;; Tell the graph package how to clone a word

(defmethod graph:clone-component ((component word))
  (make-instance 'word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command tables

(define-command-table word-table
    :inherit-from (global-claret-table
		   up-command-table
		   forward-backward-command-table
		   delete-child-command-table))

(defmethod find-claret-command-table (view (component word))
  (declare (ignore view))
  (find-command-table 'word-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands

(define-command (com-self-insert :command-table word-table) ()
  (graph:add-child (view-cursor (current-view)) *current-gesture*))

(loop for code from (char-code #\a) to (char-code #\z)
      do (set-key 'com-self-insert 'word-table `((,(code-char code)))))

(loop for code from (char-code #\A) to (char-code #\Z)
      do (set-key 'com-self-insert 'word-table `((,(code-char code)))))

(loop for code from (char-code #\0) to (char-code #\9)
      do (set-key 'com-self-insert 'word-table `((,(code-char code)))))

(define-command (com-split-word :command-table word-table) ()
  (graph:split-node (view-cursor (current-view))))

(set-key 'com-split-word 'word-table '((#\space)))

(define-command (com-split-paragraph :command-table word-table) ()
  (let ((cursor (view-cursor (current-view))))
    ;; first split the word
    (graph:split-node cursor)
    ;; then move up to the paragraph level
    (graph:move-up cursor)
    ;; split the paragraph
    (graph:split-node cursor)
    ;; go down into the new paragraph
    (graph:move-down cursor 0)))

(set-key 'com-split-paragraph 'word-table '((#\newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Making boxes

(defclass word-box (claret-boxes:hbox)
  ((%word :initarg :word :reader word)
   (%text-style :initarg :text-style :reader text-style)))

(defmethod claret-boxes:make-boxes ((component word) medium style)
  (multiple-value-bind (width height x y baseline)
      (text-size medium (graph:children component))
    (declare (ignore x y))
    (make-instance 'word-box
      :width width
      :ascent baseline
      :descent (- height baseline)
      :word component
      :text-style (medium-text-style medium))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying boxes

(defmethod claret-gui:display-box ((box word-box) pane x y)
  (let ((word (word box)))
    (with-text-style (pane (text-style box))
      (draw-text* pane (graph:children word) x y :align-y :baseline)
      (let ((cursor (view-cursor (current-view))))
	(when (eq word (graph:leaf cursor))
	  (let ((xx (text-size pane
			       (graph:children word)
			       :text-style (text-style box)
			       :start 0
			       :end (graph:location cursor))))
	    (draw-rectangle* pane
			     (1- x)
			     (- y (claret-boxes:ascent box) 1)
			     (+ x (claret-boxes:width box) 1)
			     (+ y (claret-boxes:descent box) 1)
			     :ink +yellow+
			     :filled nil)
	    (draw-line* pane (+ x xx) (+ y 3) (+ x xx) (- y 15) :ink +red+)))))))

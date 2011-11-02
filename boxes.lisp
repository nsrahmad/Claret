(in-package #:claret-boxes)

(defclass box ()
  ((%width :initarg :width :reader width)
   (%ascent :initarg :ascent :reader ascent)
   (%descent :initarg :descent :reader descent)
   (%children :initform '() :initarg :children :reader children)))

(defclass hbox (box) ())
(defclass vbox (box) ())

(defgeneric make-boxes (component medium style))

(defun make-vbox (width ascent descent children)
  (make-instance 'vbox
    :width width
    :ascent ascent
    :descent descent
    :children children))

(defun make-hbox (width ascent descent children)
  (make-instance 'hbox
    :width width
    :ascent ascent
    :descent descent
    :children children))

(defun make-empty-box (width ascent descent)
  (make-instance 'box
    :width width
    :ascent ascent
    :descent descent))

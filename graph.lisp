(in-package #:graph)

(define-condition graph-condition (condition)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Component

(defclass with-children-mixin ()
  ((%children :initform '() :initarg :children :accessor children)))

(defgeneric number-of-children (component))
(defgeneric find-child (component child-number))
(defgeneric component-add-child (component new-child child-number))
(defgeneric component-delete-child (component child-number))

(define-condition child-not-valid (graph-condition error)
  ())

(defmethod number-of-children (component)
  0)

(defmethod number-of-children ((component with-children-mixin))
  (length (children component)))

(defmethod find-child ((component with-children-mixin) child-number)
  (assert (< child-number (number-of-children component)))
  (nth child-number (children component)))

(defun sequence-type (sequence)
  (cond ((or (consp sequence) (null sequence)) 'list)
	((stringp sequence) 'string)
	((vectorp sequence) 'vector)
	(t (error "unknown sequence type"))))

(defmethod component-add-child ((component with-children-mixin) new-child child-number)
  (assert (<= child-number (number-of-children component)))
  (setf (children component)
	(concatenate (sequence-type (children component))
		     (subseq (children component) 0 child-number)
		     (list new-child)
		     (subseq (children component) child-number))))

(defmethod component-delete-child (component child-number)
  (assert (< child-number (number-of-children component)))
  (setf (children component)
	(concatenate (sequence-type (children component))
		     (subseq (children component) 0 child-number)
		     (subseq (children component) (1+ child-number)))))

(defgeneric timestamp (component))
(defgeneric (setf timestamp) (timestamp component))

(defclass component ()
  ((%timestamp :initform 0 :accessor timestamp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cursor

(defclass cursor-set ()
  ((%cursors :initform '() :initarg :cursors :accessor cursors)))

(defclass cursor ()
  ((%cursor-set :initarg :cursor-set :reader cursor-set)
   (%stack :initarg :stack :accessor stack)))

(defun make-cursor (cursor-set stack)
  (make-instance 'cursor
    :cursor-set cursor-set
    :stack stack))

(defclass with-cursor-set-mixin ()
  ((%cursor-set :initform (make-instance 'cursor-set) :reader cursor-set)))

(defun make-cursor-level (component child-number)
  (cons component child-number))

(defgeneric make-initial-cursor (component))

(defmethod make-initial-cursor ((component with-cursor-set-mixin))
  (make-cursor (cursor-set component)
	       (list (make-cursor-level component 0))))

(defun component (cursor-level)
  (car cursor-level))

(defun (setf component) (component cursor-level)
  (setf (car cursor-level) component))

(defun child-number (cursor-level)
  (cdr cursor-level))

(defun (setf child-number) (child-number cursor-level)
  (setf (cdr cursor-level) child-number))

(defgeneric in-cursor-p (component cursor))
(defgeneric leaf (cursor))
(defgeneric location (cursor))
(defgeneric clone-cursor (cursor))
(defgeneric at-beginning-p (cursor))
(defgeneric at-end-p (cursor))
(defgeneric move-up (cursor))
(defgeneric move-down (cursor child-number))
(defgeneric move-forward (cursor))
(defgeneric move-backward (cursor))
(defgeneric move-to-beginning (cursor))
(defgeneric move-to-end (cursor))
(defgeneric root (cursor))
(defgeneric mark-modified (cursor))
(defgeneric add-child (cursor new-child))
(defgeneric delete-child (cursor))
(defgeneric split-node (cursor))
(defgeneric merge-nodes (cursor))

;;; client code must define methods on this generic function
;;; if they want to invoke the split-node function
(defgeneric clone-component (component))

(defmethod root ((cursor cursor))
  (component (car (last (stack cursor)))))

(defmethod in-cursor-p (component (cursor cursor))
  (find component (stack cursor) :key #'component))

(defmethod leaf ((cursor cursor))
  (component (car (stack cursor))))

(defmethod location ((cursor cursor))
  (child-number (car (stack cursor))))

(defmethod clone-cursor ((cursor cursor))
  (let* ((set (cursor-set cursor))
	 (new-cursor (make-cursor set (stack cursor))))
    (push new-cursor (cursors set))
    new-cursor))

(defmethod at-beginning-p ((cursor cursor))
  (zerop (child-number (car (stack cursor)))))

(defmethod at-end-p ((cursor cursor))
  (let ((level (car (stack cursor))))
    (= (child-number level)
       (number-of-children (component level)))))

(defmethod move-up ((cursor cursor))
  (pop (stack cursor)))

(defmethod move-down ((cursor cursor) child)
  (assert (not (at-end-p cursor)))
  (let* ((cursor-level (car (stack cursor)))
	 (component (component cursor-level))
	 (child-number (child-number cursor-level))
	 (new-child (find-child component child-number)))
    (assert (<= child (length (children new-child))))
    (push (make-cursor-level new-child child)
	  (stack cursor))))

(defmethod move-forward ((cursor cursor))
  (assert (not (at-end-p cursor)))
  (let ((cursor-level (car (stack cursor))))
    (incf (child-number cursor-level))))

(defmethod move-backward ((cursor cursor))
  (assert (not (at-beginning-p cursor)))
  (let* ((cursor-level (car (stack cursor)))
	 (child-number (child-number cursor-level)))
    (assert (plusp child-number))
    (decf (child-number cursor-level))))

(defmethod move-to-beginning ((cursor cursor))
  (setf (child-number (car (stack cursor))) 0))

(defmethod move-to-end ((cursor cursor))
  (let ((level (car (stack cursor))))
    (setf (child-number level)
	  (number-of-children (component level)))))

(defmethod mark-modified ((cursor cursor))
  (loop with timestamp = (1+ (timestamp (root cursor)))
	for level in (stack cursor)
	do (setf (timestamp (component level)) timestamp)))

(defmethod add-child ((cursor cursor) new-child)
  (let ((level (car (stack cursor))))
    (component-add-child (component level) new-child (child-number level))
    (incf (child-number level))
    (loop for c in (cursors (cursor-set cursor))
	  do (unless (eq c cursor)
	       (let ((level2 (find (component level) (stack c) :key #'component)))
		 (unless (null level2)
		   (when (> (child-number level2) (child-number level))
		     (incf (child-number level2))))))))
  (mark-modified cursor))
    
(defmethod delete-child ((cursor cursor))
  (let ((level (car (stack cursor))))
    (component-delete-child (component level) (child-number level))
    (loop for c in (cursors (cursor-set cursor))
	  do (unless (eq c cursor)
	       (let ((level2 (find (component level) (stack c) :key #'component)))
		 (unless (null level2)
		   (cond ((> (child-number level2) (child-number level))
			  (decf (child-number level2)))
			 ((= (child-number level2) (child-number level))
			  (loop until (eq (car (stack c)) level)
				do (pop (stack c))))
			 (t nil)))))))
  (mark-modified cursor))

(defmethod split-node ((cursor cursor))
  (let* ((leaf (leaf cursor))
	 (new-node (clone-component leaf))
	 (child-number (child-number (car (stack cursor)))))
    (setf (children new-node)
	  (subseq (children leaf) child-number))
    (setf (children leaf)
	  (subseq (children leaf) 0 child-number))
    (mark-modified cursor)
    (move-up cursor)
    (move-forward cursor)
    (add-child cursor new-node)
    (move-backward cursor)
    (move-down cursor 0)
    (mark-modified cursor)
    (loop for c in (cursors (cursor-set cursor))
	  do (unless (eq c cursor)
	       (let ((level (find leaf (stack c) :key #'component)))
		 (unless (null level)
		   (when (> (child-number level) child-number)
		     (setf (component level) new-node)
		     (decf (child-number level) child-number))))))))

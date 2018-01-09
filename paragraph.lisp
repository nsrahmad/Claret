(in-package #:claret-paragraph)

(defclass paragraph (graph:component graph:with-children-mixin)
  ())

;;; Tell the graph package how to clone a paragraph

(defmethod graph:clone-component ((component paragraph))
  (make-instance 'paragraph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command tables

(define-command-table paragraph-table
    :inherit-from (global-claret-table
		   up-command-table
		   down-command-table
		   forward-backward-command-table))

(defmethod find-claret-command-table (view (component paragraph))
  (declare (ignore view))
  (find-command-table 'paragraph-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Making boxes

(defun filled-line (word-boxes available-width)
  (let* ((length (length word-boxes))
	 (total-width (reduce #'+ word-boxes :key #'claret-boxes:width))
	 (max-ascent (reduce #'max word-boxes :key #'claret-boxes:ascent))
	 (max-descent (reduce #'max word-boxes :key #'claret-boxes:descent))
	 (width-to-distribute (- available-width total-width))
	 (number-of-boxes-to-place (1- length)))
    (claret-boxes:make-hbox
     available-width max-ascent max-descent
     (cons (car word-boxes)
	   (loop for box in (cdr word-boxes)
		 for extra = (ceiling width-to-distribute number-of-boxes-to-place)
		 collect (claret-boxes:make-empty-box extra 1 0)
		 collect box
		 do (decf number-of-boxes-to-place)
		 do (decf width-to-distribute extra))))))

(defun ragged-line (word-boxes available-width space-width)
  (let* ((max-ascent (reduce #'max word-boxes :key #'claret-boxes:ascent))
	 (max-descent (reduce #'max word-boxes :key #'claret-boxes:descent)))
    (claret-boxes:make-hbox
     available-width max-ascent max-descent
     (cons (car word-boxes)
	   (loop for box in (cdr word-boxes)
		 collect (claret-boxes:make-empty-box space-width 1 0)
		 collect box)))))

(defmethod claret-boxes:make-boxes ((component paragraph) medium style)
  (let ((available-width 600)
	(space-width (text-size medium " "))
	(child-boxes (loop for child in (graph:children component)
			   collect (claret-boxes:make-boxes child medium style))))
    ;; do this better grouping the first two and the last two lines
    ;; or all lines if there are three of them.
    (loop until (null child-boxes)
	  collect (loop with total-box-width = (claret-boxes:width (car child-boxes))
			with word-boxes = (list (pop child-boxes))
			for space from 0 by space-width
			until (null child-boxes)
			until (>= (+ total-box-width
				     space
				     (claret-boxes:width (car child-boxes)))
				  available-width)
			do (incf total-box-width (claret-boxes:width (car child-boxes)))
			do (push (pop child-boxes) word-boxes)
			finally (return (if (null child-boxes)
					    (ragged-line (reverse word-boxes)
							 available-width
							 space-width)
					    (filled-line (reverse word-boxes)
							 available-width)))))))

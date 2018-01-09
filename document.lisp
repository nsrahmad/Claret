(in-package #:claret-document)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input 

(defparameter *claret-readtable* (copy-readtable))

(defun read-claret-object (stream char)
  (declare (ignore char))
  (apply #'make-instance (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'read-claret-object nil *claret-readtable*)

(set-syntax-from-char #\] #\) *claret-readtable*)

(defun read-document-from-stream (stream)
  (let* ((readtable *claret-readtable*)
	 (*read-eval* nil)
	 (*readtable* readtable))
    (read stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Output

(defgeneric slots-to-be-saved (object)
  (:method-combination append :most-specific-last))

(defmethod save-object (object stream)
  (pprint-logical-block (stream nil :prefix "[" :suffix "]")
    (format stream "~s ~2i" (class-name (class-of object)))
    (loop with slot-names = (slots-to-be-saved object)
	  for slot in (sb-mop:class-slots (class-of object))
	  do (when (member (sb-mop:slot-definition-name slot) slot-names
			   :test 'eq)
	       (format stream "~_~W ~W "
		       (car (sb-mop:slot-definition-initargs slot))
		       (slot-value object (sb-mop:slot-definition-name slot)))))))

(defparameter *save-readably* nil)

(defclass claret-object () ())

(defmethod print-object ((obj claret-object) stream)
  (if *save-readably*
      (save-object obj stream)
      (call-next-method)))

(defun write-document-to-stream (document stream)
  (let ((*print-circle* t)
        (*package* (find-package :keyword))
	(*save-readably* t))
    (pprint document stream)
    (terpri stream)
    (finish-output stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Document

(defclass document (component claret-object
		    with-cursor-set-mixin
		    with-children-mixin esa-buffer-mixin)
  ())


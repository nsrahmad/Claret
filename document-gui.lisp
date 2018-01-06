(in-package :claret-document-gui)

(declaim (optimize (debug 3)))

(defmethod display-component (pane (component document))
  (with-text-family (pane :sans-serif)
    (let ((boxes (loop for (child next) on (graph:children component)
		       append (claret-boxes:make-boxes child pane nil)
		       unless (null next)
                         append (list (claret-boxes:make-empty-box 1 30 0)))))
      (loop with x = 0
	    with y = 0
	    for box in boxes
	    do (claret-gui:display-box box pane x (+ y (claret-boxes:ascent box)))
	    do (incf y (+ (claret-boxes:ascent box) (claret-boxes:descent box)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command tables

(define-command-table document-table
    :inherit-from (global-claret-table
		   down-command-table
		   forward-backward-command-table))

(defmethod find-claret-command-table (view (component document))
  (declare (ignore view))
  (find-command-table 'document-table))

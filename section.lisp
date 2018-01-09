(in-package #:claret-section)

(defclass section (graph:component graph:with-children-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command tables.

(define-command-table section-table
    :inherit-from (global-claret-table
                   down-command-table
                   up-command-table
                   forward-backward-command-table))

(defmethod find-claret-command-table (view (component section))
  (declare (ignore view))
  (find-command-table 'section-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands and key bindings.

(define-command (com-insert-section :name t :command-table claret-document-gui:document-table)
    ()
  (let ((cursor (view-cursor (current-view))))
    (graph:add-child cursor (make-instance 'section))
    (graph:move-backward cursor)
    (graph:move-down cursor 0)
    (graph:add-child cursor (make-instance 'claret-paragraph:paragraph))
    (graph:move-backward cursor)
    (graph:move-down cursor 0)
    (graph:add-child cursor (make-instance 'claret-word:word))
    (graph:move-backward cursor)
    (graph:move-down cursor 0)))


(add-command-to-command-table
 'com-insert-section 'section-table :name t :errorp nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Making boxes.

(defmethod claret-boxes:make-boxes ((component section) medium style)
  (let ((main-boxes
         (loop for (child next) on (cdr (graph:children component))
               append (claret-boxes:make-boxes child medium style)
               unless (null next) append (list (claret-boxes:make-empty-box 1 10 0)))))
    (if (null (graph:children component))
        main-boxes
        (append (with-text-face (medium :bold)
                  (with-text-size (medium :large)
                    (claret-boxes:make-boxes
                     (car (graph:children component)) medium style)))
                (list (claret-boxes:make-empty-box 1 10 0))
                main-boxes))))

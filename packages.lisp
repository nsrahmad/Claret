(defpackage #:graph
    (:use #:cl)
  (:export #:with-children-mixin
	   #:children
	   #:component
	   #:timestamp
	   #:number-of-children
	   #:find-child
	   #:at-beginning-p
	   #:at-end-p
	   #:add-child
	   #:delete-child
	   #:clone-component
	   #:split-node
	   #:merge-nodes
	   #:with-cursor-set-mixin
	   #:make-initial-cursor
	   #:cursor-set
	   #:cursor
	   #:in-cursor-p
	   #:root
	   #:leaf
	   #:location
	   #:make-cursor
	   #:clone-cursor
	   #:move-up
	   #:move-down
	   #:move-forward
	   #:move-backward
	   #:move-to-beginning
	   #:move-to-end))

(defpackage #:claret-document
    (:use #:cl #:graph #:esa-buffer)
  (:export #:document
	   #:slots-to-be-saved
	   #:read-document-from-stream
	   #:write-document-to-stream))

(defpackage #:claret-boxes
    (:use #:clim-lisp)
  (:export #:box
	   #:make-boxes
	   #:vbox
	   #:make-vbox
	   #:hbox
	   #:make-hbox
	   #:make-empty-box
	   #:ascent
	   #:descent
	   #:width
	   #:children))

(defpackage #:claret-gui
    (:use #:clim-lisp #:clim #:esa #:esa-io #:esa-buffer #:claret-document)
  (:export #:display-frame-pane-vew
	   #:display-box
	   #:display-component
	   #:current-view
	   #:view-cursor
	   #:find-claret-command-table
	   #:global-claret-table
	   #:up-command-table
	   #:down-command-table
	   #:forward-backward-command-table
	   #:delete-child-command-table))

(defpackage #:claret-document-gui
    (:use #:clim-lisp #:clim #:claret-document #:claret-gui :esa)
  (:export #:document-table))

(defpackage #:claret-section
    (:use #:clim-lisp #:clim #:claret-document #:claret-gui :esa)
  (:export #:section
	   #:section-table))

(defpackage #:claret-paragraph
    (:use #:clim-lisp #:clim #:claret-document #:claret-gui :esa)
  (:export #:paragraph
	   #:paragraph-table))

(defpackage #:claret-word
    (:use #:clim-lisp #:clim #:claret-document #:claret-gui :esa)
  (:export #:word
	   #:word-table))

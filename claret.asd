;;;  (c) copyright 2007 by Kristian Kocher (kristian.kocher@gmail.com)
;;;
;;;  (c) copyright 2007 by Robert Strandh (robert.strandh@gmail.com)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as published by
;;; the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation
;;; 51 Franklin Street, Fifth Floor
;;; Boston, MA 02110-1301
;;; USA

;;; Claret ASDF system definition

(asdf:defsystem :claret
  :name "claret"
  :version "0.1" 
  :depends-on (:mcclim :closer-mop)
  :serial t
  :components ((:file "packages")
	       (:file "graph")
	       (:file "document")
	       (:file "boxes")
	       (:file "gui")
	       (:file "document-gui")
	       (:file "section")
	       (:file "paragraph")
	       (:file "word")))

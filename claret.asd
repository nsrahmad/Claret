;;;  (c) copyright 2007 by Kristian Kocher (kristian.kocher@gmail.com)
;;;
;;;  (c) copyright 2007 by Robert Strandh (strandh@labri.fr)
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
  :depends-on (:mcclim)
  :components ((:file "packages")
	       (:file "graph" :depends-on ("packages"))
	       (:file "document" :depends-on ("packages" "graph"))
	       (:file "boxes" :depends-on ("packages"))
	       (:file "gui" :depends-on ("packages" "document" "boxes"))
	       (:file "document-gui" :depends-on ("packages" "graph" "document" "gui"))
	       (:file "section" :depends-on ("packages" "graph" "document" "gui"))
	       (:file "paragraph" :depends-on ("packages" "graph" "document" "gui"))
	       (:file "word" :depends-on ("packages" "graph" "document" "gui"))))

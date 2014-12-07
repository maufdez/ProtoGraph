(defpackage :cl-protograph (:use :asdf :cl))
(in-package :cl-protograph)

(defsystem cl-protograph
  :name "cl-protograph"
  :author "Mauricio Fernandez <maufdez2@gmail.com>"
  :version "1.0"
  :maintainer "Mauricio Fernandez <maufdez2@gmail.com>"
  :licence "MIT"
  :description "Prototyping Graph Database."
  :long-description ""
  :components
  ((:file "proto-graph")
   (:file "proto-query" :depends-on ("proto-graph"))))

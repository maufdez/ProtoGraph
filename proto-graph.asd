(defpackage :proto-graph-db (:use :asdf :cl))
(in-package :proto-graph-db)

(defsystem proto-graph-db
  :name "proto-graph-db"
  :author "Mauricio Fernandez <maufdez2@gmail.com>"
  :version "1.0"
  :maintainer "Mauricio Fernandez <maufdez2@gmail.com>"
  :licence "MIT"
  :description "Prototyping Graph Database."
  :long-description ""
  :components
  ((:file "proto-graph")
   (:file "proto-query" :depends-on ("proto-graph"))))

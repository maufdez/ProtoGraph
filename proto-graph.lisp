(defpackage :proto-graph 
  (:use :common-lisp)
  (:export :get-keys
	   :id
	   :from-node
	   :to-node
	   :all-nodes
	   :all-links
	   :node-create
	   :node-match
	   :node-remove
	   :link-create
	   :nodes-linked-from
	   :nodes-linked-to
	   :get-prop
	   :dump-props 
	   :links-with-type
	   :links-from-node
	   :links-to-node 
	   :link-remove 
	   :link-get-types
	   :link-match
	   :clear-db))

(in-package :proto-graph)

(defvar *id-counter* 0
  "This variable will make sure that each node has a unique id.")

(defvar *nodes* nil
  "A list where all nodes will be stored.")

(defvar *links* nil 
  "A list where all links will be stored.")

(defvar *filters*
  "Used to store properties to match in links or nodes")

;;; Utilities, maybe to be moved to a different library later

;; get-keys gets every other element of a list, starting from the first.
(defun get-keys (list) 
"Get the keys from a plist"
  (if (cdr list) 
      (cons (car list) (get-keys (cddr list))) 
      nil))

;;; Class definition, we start with a thing (could not think of a better name)
;;; which parents both nodes and links, to avoid writing too many methods
;;; for things that are common to both.

(defclass thing ()
  ((properties
    :documentation "A plist with properties for a thing in the DB"
    :initarg :properties
    :accessor properties)))

(defclass node (thing)
  ((id
    :initform (prog1 *id-counter* (incf *id-counter*)) ;the prog1 is to have the value returned before increasing.
    :reader id)
   (label-list
    :documentation "List with the labels for the current node"
    :initarg :label-list
    :accessor label-list
    :initform nil)))

(defclass link (thing)
  ((type
    :initarg :type
    :initform (error "Must supply a link type")
    :reader of-type)
   (from-node
    :documentation "Links are directed will start from this node"
    :initarg :from-node
    :initform (error "Must supply an starting node")
    :reader from-node)
   (to-node
    :documentation "Links are directed end in this node"
    :initarg :to-node
    :initform (error "Must supply an starting node")
    :reader to-node)))

;;; Functions for handling properties
(defgeneric get-prop (object property)
  (:documentation "Get the value for a property in the plist"))

(defgeneric set-prop (object property value)
  (:documentation "Set the value for a property in the plist"))

(defgeneric dump-props (object)
  (:documentation "This method will print the properties was copied from Practical Common Lisp" ))

(defmethod get-prop ((object thing) property)
  (getf (properties object) property))

(defmethod set-prop ((object thing) property value)
  (setf (getf (properties object) property) value))

(defmethod dump-props ((object thing))
  (format t "~{~a:~10t~a~%~}~%" (properties object)))

(defgeneric meets-filter (object)
  (:documentation "Uses the filters stored in the special variable *filters* to check if the properties of a thing meet it"))

(defmethod meets-filter ((object thing))
  (reduce (lambda (a b)(and a b))(mapcar (lambda (p)(equal (getf *filters* p)(get-prop object p)))(get-keys *filters*))))

(defsetf get-prop set-prop)

;; Convenience functions for accessing data
(defun all-nodes () 
  "Returns the list of all nodes in the database"
  *nodes*)

(defun all-links ()
  "Returns the list of all links in the database"
  *links*)

;;; Functions and Macros for nodes
(defun node-create (&key (label :default) properties)
  ;;I use a CAR to have the function return just the node that was just added.
  (car (push (make-instance 'node :label-list (list label) :properties properties) *nodes*)))

(defun has-label (label &optional (nodes *nodes*))
  (remove-if-not (lambda (n)(find label (label-list n))) nodes))

(defun node-match (&key label properties (nodes *nodes*))
  (let ((*filters* properties))
    (if label (if properties (remove-if-not #'meets-filter (has-label label nodes))
		  (has-label label nodes))
	(if properties (remove-if-not #'meets-filter nodes)
	    nodes))))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (id properties) object
      (format stream "ID: ~a ~{|~a: ~a ~}" id properties))))

(defmethod print-object ((object link) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (type from-node to-node) object
      (format stream "~a [~a]  ~a" from-node type to-node))))

(defun node-remove (node)
  "Removes the given node from the list and all links to and from it"
  (progn  
    (setf *links* (remove-if #'(lambda (l) (or (eq (from-node l) node) (eq (to-node l) node))) *links*))
    (setf *nodes* (remove node *nodes*))))

(defun links-with-type (type &optional (list *links*))
  "Returns a list of only links with a specified type from a list"
  (remove-if-not (lambda (n)(equal type (of-type n))) list))

(defun links-to-node (node &optional (list *links*))
  "Retursn a list of only links ending at a specified node from a list"
  (remove-if-not #'(lambda (n) (eq node (to-node n))) list))

(defun nodes-linked-to (node &key link-type (link-list *links*))
  (mapcar #'from-node (if link-type (links-with-type link-type (links-to-node node link-list))
			  (links-to-node node link-list))))

(defun links-from-node (node &optional (list *links*))
  "Retursn a list of only links starting at a specified node from a list"
  (remove-if-not #'(lambda (n) (eq node (from-node n))) list))

(defun nodes-linked-from (node &key link-type (link-list *links*))
  (mapcar #'to-node (if link-type (links-with-type link-type (links-from-node node link-list))
			  (links-from-node node link-list))))

;;; Functions and Macros for links
(defun link-create (type from-node to-node &key properties)
  (if (eq from-node to-node)
      (error "From and to nodes can not be equal")
      (car (push (make-instance 'link :type type :from-node from-node :to-node to-node :properties properties) *links*))))

;; There could be more than one link conecting two nodes so the type is required.
(defun link-remove (node-type node-from node-to)
  "Revove a link given the type starting and ending nodes"
  (setf *links* (remove-if #'(lambda (l) (and (eq (from-node l) node-from) 
					      (eq (to-node l) node-to) 
					      (eq (of-type l) node-type))) *links*)))

(defun link-get-types (from-node to-node)
  (mapcar #'of-type (links-from-node from-node (links-to-node to-node))))

(defun link-match (&key of-type properties (links *links*))
  (let ((*filters* properties))
    (if of-type (if properties (remove-if-not #'meets-filter (links-with-type of-type links))
		  (links-with-type of-type links))
	(if properties (remove-if-not #'meets-filter links)
	    links))))

;;; General database management.
(defun clear-db ()
  "Clears the dabase nodes and links"
  (setf *nodes* nil)
  (setf *links* nil))

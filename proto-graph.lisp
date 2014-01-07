(defvar *id-counter* 0
  "This variable will make sure that each node has a unique id.")

(defvar *nodes* nil
  "A list where all nodes will be stored.")

(defvar *links* nil 
  "A list where all links will be stored.")

(defvar *filters*
  "Used to store properties to match in links or nodes")

(defclass thing ()
  ((properties
    :documentation "A plist with properties for a thing in the DB"
    :initarg :properties
    :accessor properties)))


;;; Class definition, we start with a thing (could not think of a better name)
;;; which parents both nodes and links, to avoid writing too many methods
;;; for things that are common to both.

(defclass node (thing)
  ((id
    :initform (prog1 *id-counter* (incf *id-counter*))
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
    :reader type)
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

(defmethod get-prop ((object thing) property)
  (getf (properties object) property))

(defmethod set-prop ((object thing) property value)
  (setf (getf (properties object) property) value))

(defgeneric meets-filter (object)
  (:documentation "Uses the filters stored in the special variable *filters* to check if the properties of a method meet it"))

(defmethod meets-filter ((object thing))
  (reduce (lambda (a b)(and a b))(mapcar (lambda (p)(equal (getf *filters* p)(get-prop object p)))(remove-if-not #'symbolp *filters*))))

;;; Functions and Macros for nodes
(defun node-create (&key (label :default) properties)
  (push (make-instance 'node :label-list (list label) :properties properties) *nodes*))

(defun has-label (label)
  (remove-if-not (lambda (n)(find label (label-list n))) *nodes*))

(defun node-match (&key label properties)
  (let ((*filters* properties))
    (if label (if properties (remove-if-not #'meets-filter (has-label label))
		  (has-label label))
	(if properties (remove-if-not #'meets-filter *nodes*)
	    *nodes*))))

;;; Functions and Macros for links
(defun link-create (type from-node to-node &key properties)
  (if (eq from-node to-node)
      (error "From and to nodes can not be equal")
      (push (make-instance 'link :type type :from-node from-node :to-node to-node :properties properties) *links*)))

(defun has-type (type)
  (remove-if-not (lambda (n)(equal type (type n))) *links*))

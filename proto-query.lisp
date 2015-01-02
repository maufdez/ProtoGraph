(defpackage :proto-query
  (:use :common-lisp :proto-graph)
  (:export :rec-search
	   :make-deep-searcher
	   :make-safe-deep-searcher
	   :deep-rec-search))

(in-package :proto-query)

(defun rec-search (dir nodes &key (link-type nil)(link-list (all-links)))
  "Looks for all nodes connected in the given direction (:to or :from) by links of linktype or any type if not given"
  (let ((funct (cond ((eq dir :to) #'nodes-linked-to)
		     ((eq dir :from) #'nodes-linked-from)
		     (t (error "Direction must be :to or :from")))))
    (if (consp nodes) (if nodes (nconc (funcall funct (car nodes) :link-type link-type :link-list link-list)
				       (rec-search dir (cdr nodes) :link-type link-type :link-list link-list))
			  nil)
	(funcall funct nodes :link-type link-type :link-list link-list))))



(defun make-deep-searcher (dir nodes &key (link-type nil) (link-list (all-links)))
  "Creates a closure which will recursively look for nodes connected in the given dir (:to or :from) with liktyp, or any type of link if not given"
  (let ((depth 0) (current-nodes nodes))
    (lambda () (if current-nodes (values (incf depth) 
					 (setf current-nodes (rec-search dir current-nodes :link-type link-type :link-list link-list)))
		   (values depth current-nodes)))))

(defun make-safe-deep-searcher (dir nodes &key (link-type nil) (link-list (all-links)))
  "Same as make-deep-searcher, but keeps a record of visited nodes and avoids visiting them again (endless cycle protection)"
  (let ((visited ())
	(depth 0)
	(current-nodes (if (atom nodes) (list nodes) nodes)))
    (lambda () (progn
		 (when visited (setf current-nodes (delete-if #'(lambda (e) (find e visited)) current-nodes)))
		 (mapcar #'(lambda (e) (pushnew e visited)) current-nodes)
		 (if current-nodes (values (incf depth) 
					   (setf current-nodes (rec-search dir current-nodes :link-type link-type :link-list link-list)))
		     (values depth current-nodes))))))

(defun deep-rec-search (dir nodes depth &key (link-type nil) (link-list (all-links)))
  "Takes a direction which can be :to or :from, a list of nodes or a single node, a depth and optionally a link type
   it does return a list of all nodes that follow a linktype relationship (or any type of relationship, if linktype
   is not provided, upto the given depth, without going into already visited nodes"
  (let ((search (make-safe-deep-searcher dir nodes :link-type link-type :link-list link-list)))
    (do (final result)(final result) 
      (multiple-value-bind (cur-depth ret-nodes) (funcall search) 
	(setf result (nconc result ret-nodes))
	(setf final (or (= cur-depth depth) (not ret-nodes)))))))

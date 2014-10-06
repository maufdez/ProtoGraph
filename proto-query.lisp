(defpackage :proto-query
  (:use :common-lisp :proto-graph)
  (:export :rec-search
	   :make-deep-searcher
	   :make-safe-deep-searcher
	   :deep-rec-search))

(in-package :proto-query)

(defun rec-search (dir nodes &optional linktype)
  (let ((funct (cond ((eq dir :to) #'nodes-linked-to)
		     ((eq dir :from) #'nodes-linked-from)
		     (t (error "Direction must be :to or :from")))))
    (if (consp nodes) (if nodes (append (funcall funct (car nodes) linktype)(rec-search dir (cdr nodes) linktype))
			  nil)
	(funcall funct nodes linktype))))

(defun make-deep-searcher (dir nodes &optional linktype)
  (let ((depth 0) (current-nodes nodes))
    (lambda () (if current-nodes (values (incf depth) (setf current-nodes (rec-search dir current-nodes linktype)))
		   (values depth current-nodes)))))

(defun make-safe-deep-searcher (dir nodes &optional linktype)
  (let ((visited ())
	(depth 0)
	(current-nodes (if (atom nodes) (list nodes) nodes)))
    (lambda () (progn
		 (when visited (setf current-nodes (delete-if #'(lambda (e) (find e visited)) current-nodes)))
		 (mapcar #'(lambda (e) (pushnew e visited)) current-nodes)
		 (if current-nodes (values (incf depth) (setf current-nodes (rec-search dir current-nodes linktype)))
		     (values depth current-nodes))))))

(defun deep-rec-search (dir nodes depth &optional linktype)
  (let ((search (make-safe-deep-searcher dir nodes linktype)))
    (do (final result)(final result) 
      (multiple-value-bind (cur-depth ret-nodes) (funcall search) 
	(setf result (nconc result ret-nodes))
	(setf final (or (= cur-depth depth) (not ret-nodes)))))))

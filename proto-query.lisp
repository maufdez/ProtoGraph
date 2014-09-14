(defpackage :proto-query
  (:use :common-lisp :proto-graph)
  (:export :rec-search))

(defun rec-search (dir nodes &optional linktype)
  (let ((funct (cond ((eq dir :to) #'nodes-linked-to)
		     ((eq dir :from) #'nodes-linked-from)
		     (t (error "Direction must be :to or :from")))))
    (if (consp nodes) (if nodes (append (funcall funct (car nodes) linktype)(rec-search dir (cdr nodes) linktype))
			  nil)
	(funcall funct nodes linktype))))

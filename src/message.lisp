(in-package :lweb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))

(defmacro build-message-accessors ((&rest keys))
  (let ((msg (gensym)))
    `(progn
       ,@(mapcar #'(lambda (key)
		     `(defmacro ,(symb 'message- key) (,msg)
			`(getf ,,msg ,',key)))
		 keys))))

(build-message-accessors (:id
			  :text
			  :author-id
			  :header
			  :visible
			  :parent-id
			  :parent
			  :thread
			  :author
			  :root-id
			  :children-ids))

(defmacro make-message (&key 
			id
			(text "Hello world")
			(author-id 1)
			(header "Hello")
			(visible t)
			(parent-id nil))
  (let ((root-id (gensym)))
    `(let ((,root-id (if ,parent-id
			(message-root-id (get-message ,parent-id))
			,id)))
       (list :author-id ,author-id
	     :id ,id
	     :header ,header
	     :text ,text
	     :visible ,visible
	     :children-ids nil
	     :root-id ,root-id))))

(defvar *messages* (make-hash-table))
(defvar *last-message-id* 0)

(defun clear-messages ()
  (clrhash *messages*)
  (setf *last-message-id* 0))

(defmacro get-message (id)
  `(gethash ,id *messages*))

(defun add-message (&key 
		    (text "Hello world")
		    (author-id 1)
		    (header "Hello")
		    (visible t)
		    (parent-id nil))
  (let* ((id (incf *last-message-id*))
	 (message (make-message :id id
				:text text
				:author-id author-id
				:header header
				:visible visible
				:parent-id parent-id)))
    (setf (gethash id *messages*) message)
    (if parent-id
	(push id 
	      (message-children-ids (gethash parent-id *messages*))))
    id))

(defun get-user (id)
  (list :id id
	:name "anonymous"))


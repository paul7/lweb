(in-package #:lweb)

(defclass db-storage ()
  ((spec :initarg :spec
	 :reader  db-storage-spec)))

(defparameter *db-spec* '("lispdb" "lisp" "lisp" "localhost" :pooled-p t))

(defparameter *db-storage* 
  (make-instance 'db-storage :spec *db-spec*))

(defparameter *message-class* 'message)

(defparameter *user-class* 'user)

(defmacro with-storage (storage &body body)
  `(with-connection (db-storage-spec ,storage)
     ,@body))

(defmacro ensure-connection (&body body)
  `(if *database*
       (progn ,@body)
       (with-storage ,(symbolicate '*db-storage*)
	 ,@body)))

(defmacro defprepared/named (name (&rest args) 
			     (query &optional (format :rows)) 
			     &body body)
  (let ((prepared-name (gensym (symbol-name name)))
	(required-args (parse-ordinary-lambda-list args))
	(execute-query (symbolicate 'execute-query)))
    (with-gensyms (gresult grun-p)
      `(progn 
	 (defprepared ,prepared-name ,query ,format)
	 (defun ,name ,args
	   (let ((,grun-p nil))
	     (macrolet ((,execute-query (&rest args)
			  `(progn 
			     (setf ,',grun-p t)
			     (,',prepared-name 
			      ,@(or args ',required-args)))))
	       (let ((,gresult (progn ,@body)))
		 (declare (ignorable ,gresult))
		 (if ,grun-p
		     ,gresult
		     (,execute-query))))))))))

(defun create-table-for-class (class)
  (ensure-connection
    (execute (dao-table-definition class))
    (values)))

(defun drop-table-for-class (class)
  (ensure-connection 
    (execute (:drop-table class))
    (values)))

(defun install ()
  (ensure-connection 
    (mapc #'create-table-for-class '(message user))
    (make-anonymous))
  (values))

(defun uninstall ()
  (if (yes-or-no-p "This operation will purge all data. Proceed?")
      (ensure-connection
	(mapc #'drop-table-for-class '(message user))))
  (values))

(defun make-instances (class inits)
  (iter (for init in inits)
	(collect (apply #'make-instance class init))))

(defgeneric make-dao (class &rest args &key &allow-other-keys))

(defmethod make-dao ((class symbol) &rest args &key &allow-other-keys)
  (apply #'make-dao (find-class class) args))

(defmethod make-dao ((class dao-class) &rest args &key &allow-other-keys)
  (let ((instance (apply #'make-instance class args)))
    (insert-dao instance)))

(defmacro define-finalize-dao ((class &rest keyword-args) &body body)
  (with-gensyms (gargs)
    `(defmethod make-dao :around ((class (eql ',class)) 
				  &rest ,gargs 
				  &key ,@keyword-args &allow-other-keys)
       (declare (ignorable ,gargs))
       (let ((dao (call-next-method)))
	 ,@body
	 (update-dao dao)))))

(defun build-tree (msg-id elements)
  (when elements
    (let* ((root nil)
	   (msg-in-tree nil)
	   (parents (copy-seq elements))
	   (root-id (render-root-id (car elements))))
      (iter (for each in parents) 
	    (let ((id (render-id each)))
	      (multiple-value-bind (ours theirs)
		  (split-on #'(lambda (each)
				(= (render-parent-id each) id))
			    elements)
		(setf (message-children~ each) ours)
		(iter (for child in ours)
		      (setf (message-parent~ child) each))
		(setf elements theirs))
	      (when (= id msg-id)
		(setf msg-in-tree each))
	      (when (= id root-id)
		(setf root each))))
      (when msg-in-tree
	(if (or (= msg-id root-id)
		(message-parent~ msg-in-tree))
	    (setf (message-thread~ msg-in-tree) root)
	    (setf msg-in-tree nil)))
      msg-in-tree)))

(defprepared/named db-root-ids (user limit) ("
select m.id from 
	message m
left join 
	(select message_id 
	from ignored_message
	where user_id = $3) i
on 
	m.id = i.message_id
where 
	parent_id = 0
and 
	i.message_id is null
and
	($2
       	or
	visible)
order by id desc
limit $1
" :column)
  (let ((uid (user-id user))
	(moderator-p (user-can-moderate user)))
    (sort (execute-query limit moderator-p uid) #'>)))

(defprepared/named db-root-ids-around (id user limit) ("
(select id 
from 
	message m
left join 
	(select message_id 
	from 
		ignored_message
	where 
		user_id = $4) i
on 
	m.id = i.message_id
where 
	m.parent_id = 0
and
	i.message_id is null
and
	($3
	or
	m.visible)
and
	m.id < $1
order by m.id desc
limit $2)
	union
(select m.id 
from 
	message m
left join 
	(select message_id 
	from 
		ignored_message
	where 
		user_id = $4) i
on 
	m.id = i.message_id
where 
	m.parent_id = 0
and
	i.message_id is null
and
	($3
	or
	m.visible)
and
	m.id >= $1
order by m.id
limit $2)
" :column)
  (let ((uid (user-id user))
	(moderator-p (user-can-moderate user))
	(half-limit (ceiling (/ limit 2))))
    (sort (execute-query id half-limit moderator-p uid) #'>)))

(defprepared/named db-messages-in-thread (id user &key reverse) ("
select * 
from 
	message m
	left join
	(select message_id 
	from ignored_message
	where user_id = $2) i
	on (m.id = i.message_id)
where
	m.root_id = (select root_id from message where id = $1)
and
	i.message_id is null
order by $3 * m.id
" :plists)
  (let ((uid (user-id user)))
    (execute-query id uid (if reverse -1 1))))

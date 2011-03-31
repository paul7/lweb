(in-package #:lweb)

(defclass db-storage ()
  ((spec :initarg :spec
	 :reader  db-storage-spec)))

(defparameter *db-spec* '("lispdb" "lisp" "lisp" "localhost" :pooled-p t))

(defparameter *db-storage* 
  (make-instance 'db-storage :spec *db-spec*))

(defparameter *message-class* 'message)

(defparameter *user-class* 'user)

(defmacro ensure-connection (&body body)
  `(if *database*
       (progn ,@body)
       (with-connection (db-storage-spec *db-storage*)
	 ,@body)))

(defmacro defprepared/named (name (&rest args) 
			     (query &optional (format :rows)) 
			     &body body)
  (let ((prepared-name (gensym (symbol-name name)))
	(required-args (parse-ordinary-lambda-list args)))
    (with-gensyms (gresult gnone)
      `(progn 
	 (defprepared ,prepared-name ,query ,format)
	 (defun ,name ,args
	   (let ((,gresult ',gnone))
	     (macrolet ((execute-query (&rest args)
			  `(setf ,',gresult 
				 (,',prepared-name 
				  ,@(or args ',required-args))))
			(return-from-query (&optional result)
			  `(setf ,',gresult ,result)))
	       ,@body
	       (if (eq ,gresult ',gnone)
		   (execute-query)
		   ,gresult))))))))

(defmacro defmake (object/class &body body)
  (destructuring-bind (object &optional (class object)) 
      (ensure-list object/class)
    `(defun ,(symbolicate 'make- class) (&rest args)
       (let ((,object (apply #'make-instance ',class args)))
	 (ensure-connection
	   (insert-dao ,object)
	   ,@(if body
		 `(,@body
		   (update-dao ,object))))))))

(defmacro defclear (class)
  `(defun ,(symbolicate 'clear- class) ()
       (ensure-connection 
	 (execute (:delete-from ',class))
	 (values))))

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

(defprepared/named db-root-ids (limit moderator-p uid) ("
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
" :column))
  

(defprepared/named db-root-ids-around (id limit moderator-p uid) ("
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
" :column))

(defprepared/named db-messages-in-thread (id uid) ("
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
order by m.id
" :plists))

(defprepared/named db-messages-in-thread/reverse (id uid) ("
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
order by m.id desc
" :plists))

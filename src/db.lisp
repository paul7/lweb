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

(defun create-table-for-class (class)
  (with-storage *db-storage*
    (execute (dao-table-definition class))
    (values)))

(defun drop-table-for-class (class)
  (with-storage *db-storage*
    (execute (:drop-table class))
    (values)))

(defun install ()
  (with-storage *db-storage*
    (mapc #'create-table-for-class '(message user))
    (make-anonymous))
  (values))

(defun uninstall ()
  (if (yes-or-no-p "This operation will purge all data. Proceed?")
      (with-storage *db-storage*
	(mapc #'drop-table-for-class '(message user))))
  (values))

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

(defprepared-with-names db-root-ids (user limit) ("
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
" limit (user-can-moderate user) (user-id user))
  :column)

(defprepared-with-names db-root-ids-around (id user limit) ("
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
" id (ceiling (/ limit 2)) (user-can-moderate user) (user-id user))
  :column)

(defprepared-with-names db-messages-in-thread (id user &key reverse) ("
select m.* 
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
" id (user-id user) (if reverse -1 1))
  (:dao message))

(in-package #:lweb)

(defparameter *db-spec* '("lispdb" "lisp" "lisp" "localhost" :pooled-p t))

(defparameter *message-class* 'message)

(defparameter *user-class* 'user)

(defmacro ensure-connection (&body body)
  `(if *database*
       (progn ,@body)
       (with-connection *db-spec*
	 ,@body)))

(defmacro defmake (class)
  `(defun ,(symbolicate 'make- class) (&rest args)
     (let ((msg (apply #'make-instance ',class args)))
       (ensure-connection
	 (insert-dao msg)))))

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

(defprepared db-init-message "
select * from message 
where id = $1
"
  :plist)

(defprepared db-root-ids "
select id from message
where 
	parent_id = 0
order by id desc
limit $1
"
  :column)

(defprepared db-root-ids-around "
(select id from message
where 
	parent_id = 0
	and
	id < $1
order by id desc
limit $2)
	union
(select id from message
where 
	parent_id = 0
	and
	id >= $1
order by id
limit $2)
"
  :column)

(defprepared db-messages-in-thread "
select * from message
where
	root_id = $1
order by id
"
  :plists)
    
(defprepared db-messages-in-thread/reverse "
select * from message
where 
	root_id = $1
order by id desc
"
  :plists)
    

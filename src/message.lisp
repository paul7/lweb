(in-package :lweb)

(defclass message ()
  ((id :col-type serial 
       :initarg :id 
       :accessor message-id)
   (text :col-type text 
	 :initform "hello world" 
	 :initarg :text 
	 :accessor message-text)
   (header :col-type text 
	   :initform "hello" 
	   :initarg :header 
	   :accessor message-header)
   (visible :col-type boolean
	    :initform t 
	    :initarg :visible 
	    :accessor message-visible)
   (parent-id :col-type integer
	      :initform 0
	      :initarg :parent-id 
	      :accessor message-parent-id
	      :foreign-key (message id))
   (root-id :col-type integer 
	    :initform 0
	    :initarg :root-id 
	    :accessor message-root-id
	    :foreign-key (message id))
   (author-id :col-type integer 
	      :initform 0 
	      :initarg :author-id
	      :accessor message-author-id))
  (:keys id)
  (:metaclass dao-class))

(defmethod initialize-instance :after ((msg message) &key)
  (if (and (zerop (message-root-id msg))
	   (not (zerop (message-parent-id msg))))
      (setf (message-root-id msg) 
	    (message-root-id* (get-message (message-parent-id msg))))))

(defmacro make-message (&rest args)
  (let ((msg (gensym)))
    `(let ((,msg (make-instance 'message ,@args)))
       (ensure-connection
	 (insert-dao ,msg)))))
     
(defun clear-messages ()
  (ensure-connection 
    (mapc #'delete-dao (select-dao 'message))
    (values)))

(defun get-message (id)
  (ensure-connection
    (car (select-dao 'message (:= 'id id)))))

(defun message-author (message)
  (get-user (message-author-id message)))

(defun message-root-id* (message)
  (let* ((root-id (message-root-id message))
	 (root-id* (if (zerop root-id)
		       (message-id message)
		       root-id)))
    root-id*))
    
(defmethod render-default ((object message))
  (build-render-list :message (:id :text :header :visible :root-id :author) 
		     object))
  

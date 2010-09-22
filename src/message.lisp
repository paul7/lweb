(in-package #:lweb)

(defclass message ()
  ((id        :col-type serial 
	      :accessor message-id)
   (text      :col-type text 
	      :initform "hello world" 
	      :initarg :text 
	      :accessor message-text)
   (header    :col-type text 
	      :initform "hello" 
	      :initarg :header 
	      :accessor message-header)
   (visible   :col-type boolean
	      :initform t 
	      :initarg :visible 
	      :accessor message-visible)
   (parent-id :col-type integer
	      :initform 0
	      :initarg :parent-id 
	      :accessor message-parent-id
	      :foreign-key (message id))
   (root-id   :col-type integer 
	      :initform 0
	      :initarg :root-id 
	      :accessor message-root-id
	      :foreign-key (message id))
   (author-id :col-type integer 
	      :initform 1
	      :initarg :author-id
	      :accessor message-author-id)
   (children~ :initform nil
	      :initarg :children
	      :accessor message-children~)
   (thread~   :initform nil
	      :initarg :thread
	      :accessor message-thread~))
  (:keys id)
  (:metaclass dao-class))

(defmethod initialize-instance :after ((msg message) &key)
  (if (and (zerop (message-root-id msg))
	   (not (zerop (message-parent-id msg))))
      (setf (message-root-id msg) 
	    (message-root-id* (get-message (message-parent-id msg))))))

(defmake message)

(defclear message)

(defget message)
 
(defun get-root-message-ids ()
  (ensure-connection 
    (query (:order-by (:select 'id :from *message-class* :where (:= 'parent-id 0)) 'id)
	   :column)))
    
(defmethod message-author (message)
  (render-default (get-user (message-author-id message))))

(defun message-root-id* (message)
  (let* ((root-id (message-root-id message))
	 (root-id* (if (zerop root-id)
		       (message-id message)
		       root-id)))
    root-id*))
    
(defun get-message* (id)
  (multiple-value-bind (thread msg) (build-tree id)
    (when msg
      (setf (message-thread~ msg) thread)
      msg)))

(defun map-subthread (fn msg)
  (funcall fn msg)
  (mapcar #'(lambda (child)
	      (map-subthread fn child))
	  (message-children~ msg)))

(defmethod render-default ((object message))
  (build-render-list :message (:id :text :header :visible :root-id :author) 
		     object))
  

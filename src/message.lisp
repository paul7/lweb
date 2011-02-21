(in-package #:lweb)

(defclass message-mixin ()
  ((children~ :initform nil
	      :initarg :children
	      :accessor message-children~)
   (thread~   :initform nil
	      :initarg :thread
	      :accessor message-thread~)))

(define-option-group :message-default
  :id
  :text
  :header
  :visible
  :root-id
  :author)

(defmethod render-login ((message message-mixin))
  (restas:genurl 'login-form :id (render-id message)))

(defmethod render-url ((message message-mixin))
  (restas:genurl 'message-view :id (render-id message)))

(defmethod render-moderatorial ((message message-mixin))
  (let* ((id (render-id message))
	 (return-self (hunchentoot:url-encode
		       (restas:genurl 'message-view :id id)))
	 (return-parent (hunchentoot:url-encode 
			 (restas:genurl 'message-view :id (render-parent-id message)))))
    (if (user-can-moderate (ensure-auth *current-user*))
	(nconc
	 (list (if (render-visible message)
		   (list :caption "Hide"
			 :route (restas:genurl 'hide 
					       :id id
					       :return return-self))
		   (list :caption "Show"
			 :route (restas:genurl 'show 
					       :id id
					       :return return-self)))) 
	 (list (list :caption "Delete"
		     :route (restas:genurl 'erase 
					   :id id
					   :return return-parent)))))))

(defmethod render-children ((message message-mixin))
  (let ((children (message-children~ message)))
    (iter (for child in children)
	  (collect (render (:message-default 
			    :children 
			    :url 
			    :moderatorial) child)))))

(defmethod render-thread ((message message-mixin))
  (render (:message-default 
	   :children 
	   :url
	   :moderatorial) (message-thread~ message)))

(defmethod render-posturl ((message message-mixin))
  (restas:genurl 'message-post :parent (render-id message)))
  
(defmethod render-index ((object t))
  (restas:genurl 'message-list))
  
(defmethod render-around ((message message-mixin))
  (restas:genurl 'message-list-around :id (render-id message)))

(defclass message (message-mixin)
  ((id        :col-type serial
	      :initarg :id
	      :accessor message-id
	      :reader   render-id)
   (text      :col-type text 
	      :initform "hello world" 
	      :initarg :text 
	      :accessor message-text
	      :reader   render-text)
   (header    :col-type text 
	      :initform "hello" 
	      :initarg :header 
	      :accessor message-header
	      :reader   render-header)
   (visible   :col-type boolean
	      :initform t 
	      :initarg :visible 
	      :accessor message-visible
	      :reader   render-visible)
   (parent-id :col-type integer
	      :initform 0
	      :initarg :parent-id 
	      :accessor message-parent-id
	      :reader   render-parent-id
	      :foreign-key (message id))
   (root-id   :col-type integer 
	      :initform 0
	      :initarg :root-id 
	      :accessor message-root-id
	      :reader   render-root-id
	      :foreign-key (message id))
   (author-id :col-type integer 
	      :initform 1
	      :initarg :author-id
	      :accessor message-author-id))
  (:keys id)
  (:metaclass dao-class))

(defmethod initialize-instance :after ((msg message) &key)
  (if (and (zerop (message-root-id msg))
	   (not (zerop (message-parent-id msg))))
      (setf (message-root-id msg) 
	    (render-root-id* (get-message (message-parent-id msg))))))

(defclass ignored-message ()
  ((user-id    :col-type integer
	       :initform 0
	       :initarg :user-id
	       :accessor ignored-message-user-id)
   (message-id :col-type integer 
	       :initform 0
	       :initarg :user-id
	       :accessor ignored-message-message-id))
  (:keys user-id message-id)
  (:metaclass dao-class))

(defmethod init-message ((class (eql 'message)) id)
  (db-init-message id))

(defun get-message (id &key (class *message-class*))
  (ensure-connection
    (let ((init (init-message class id)))
      (when init
	(apply #'make-instance class init)))))

(defmake message)

(defclear message)

(defun get-root-message-ids (&key around (limit *index-limit*))
  (ensure-connection 
    (if around
	(let ((half-limit (ceiling (/ limit 2))))
	  (sort
	   (db-root-ids-around around half-limit)
	   #'>))
	(db-root-ids limit))))
    
(defmethod render-author ((message message-mixin))
  (render (:user-default) (get-user (message-author-id message))))

(defun render-root-id* (message)
  (let* ((root-id (render-root-id message))
	 (root-id* (if (zerop root-id)
		       (render-id message)
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


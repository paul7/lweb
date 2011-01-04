(in-package #:lweb)

(defparameter *current-user* nil)

(defun get-current-user ()
  (let* ((cookie-id (parse-integer 
		     (or (hunchentoot:cookie-in "uid") "1") 
		     :junk-allowed t))
	 (user (get-user cookie-id)))
    (or user (user-anonymous))))

(defmacro ensure-auth (&body body)
  `(if *current-user*
       (progn ,@body)
       (let ((*current-user* (get-current-user)))
         ,@body)))

(defmacro define-message-action (name &body body)
  (let ((route (concatenate 'string
			    (string-downcase (symbol-name name))
			    "/:id/:return")))
    `(restas:define-route ,name (,route
				 :parse-vars (list :id #'parse-integer))
       (if (user-can-moderate (ensure-auth *current-user*))
	   (let ((message (get-message* id)))
	     (if message
		 (progn 
		   ,@body
		   (restas:redirect return))
		 (restas:redirect 'access-denied)))
	   (restas:redirect 'access-denied)))))

(defun message-thread (message)
  (render (:message-default 
	   :children 
	   :url
	   :moderatorial) (message-thread~ message)))

(define-class-options message
  (:login   
   (restas:genurl 'login-form :id (message-id message)))

  (:url
   (restas:genurl 'message-view :id (message-id message)))

  (:moderatorial 
   (let* ((id (message-id message))
	  (return-self (hunchentoot:url-encode
			(restas:genurl 'message-view :id id)))
	  (return-parent (hunchentoot:url-encode 
			  (restas:genurl 'message-view :id (message-parent-id message)))))
     (if (user-can-moderate (ensure-auth *current-user*))
	 (nconc
	  (list (if (message-visible message)
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

  (:children
   (let ((children (message-children~ message)))
     (iter (for child in children)
	   (collect (render (:message-default 
			     :children 
			     :url 
			     :moderatorial) child)))))

  (:thread
   (message-thread message))

  (:user
   (ensure-auth
     (render (:user-default) *current-user*)))

  (:writable
   (ensure-auth
     (user-can-post *current-user*)))

  (:posturl
   (restas:genurl 'message-post :parent (message-id message)))
  
  (:index
   (restas:genurl 'message-list))
  
  (:around
   (restas:genurl 'message-list-around :id (message-id message))))

(defun root-posturl ()
  (restas:genurl 'start-thread :parent 0))

(defun message-visible* (msg)
  (or (message-visible msg)
      (ensure-auth
	(user-can-moderate *current-user*))))

(defun build-tree (msg-id)
  (let ((msg (get-message msg-id)))
    (if msg
	(let* ((root-id (message-root-id* msg))
	       (root (get-message root-id))
	       (msg-in-tree nil)
	       (elements 
		(ensure-auth
		  (remove-if-not #'message-visible*
				 (cons root 
				       (ensure-connection 
					 (if *reverse-order* 
					     (select-dao *message-class* 
							 (:= 'root-id root-id)
							 (:desc 'id))
					     (select-dao *message-class* 
							 (:= 'root-id root-id)
							 'id)))))))
	       (parents (copy-seq elements)))
	  (mapc #'(lambda (each)
		    (let ((id (message-id each)))
		      (if (= id msg-id)
			  (setf msg-in-tree each))
		      (multiple-value-bind (ours theirs)
			  (split-on #'(lambda (each)
					(= (message-parent-id each) id))
				    elements)
			(setf (message-children~ each) ours)
			(setf elements theirs))))
		parents)
	  (values root
		  msg-in-tree)))))

(defun message-post-check (&key parent-id header text)
  (declare (ignore text parent-id))
  (plusp (length header)))


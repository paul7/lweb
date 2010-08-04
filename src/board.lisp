(in-package :lweb)

(restas:define-route message-view (":id"
				   :parse-vars (list :id #'parse-integer))
  (let ((msg (get-message id)))
    (if msg
	(if (message-visible* msg)
	    (render :message (:login :thread :user) msg)
	    (restas:redirect 'access-denied))
	hunchentoot:+http-not-found+)))

(restas:define-route message-list ("index")
  (let ((messages (ensure-connection 
		    (select-dao 'message (:= 'parent-id 0)))))
    (list :messages (mapcar #'message-thread messages))))

(restas:define-route message-post (":parent"
				   :method :post
				   :requirement #'(lambda ()
						    (hunchentoot:post-parameter "send"))
				   :parse-vars (list :parent #'parse-integer))
  (let ((header (hunchentoot:post-parameter "header"))
	(text (hunchentoot:post-parameter "text"))
	(user (ensure-auth 
		*current-user*)))
    (if (user-can-post user)
	(if (message-post-check :parent-id parent
				:header header
				:text text)
	    (progn
	      (make-message :parent-id parent
			    :header header
			    :text text
			    :visible (user-can-post-postmoderated user)
			    :author-id (user-id user))
	      (restas:redirect 'message-view 
			       :id parent))
	    (list :error "empty topic"
		  :return parent))
	(restas:redirect 'access-denied))))

(restas:define-route access-denied ("stop")
  (list :reason 42))
	
(restas:define-route login-form ("login/:id")
  (declare (ignore id)))

(restas:define-route login-as-uid ("login/:id"
				 :method :post
				 :requirement #'(lambda ()
						  (hunchentoot:post-parameter "login")))
  (let ((new-id (or 
		 (parse-integer (hunchentoot:post-parameter "uid") :junk-allowed t)
		 0)))
    (hunchentoot:set-cookie "uid" 
			    :value (format nil "~a" new-id)
			    :path "/"
			    :http-only t))
  (restas:redirect 'message-view :id id))

(restas:define-route show ("show/:id"
			   :parse-vars (list :id #'parse-integer))
  (if (user-can-moderate (ensure-auth *current-user*))
      (let ((msg (get-message id)))
	(setf (message-visible msg) t)
	(ensure-connection 
	  (update-dao msg))
	(restas:redirect 'message-view :id id))
      (restas:redirect 'access-denied)))

(restas:define-route hide ("hide/:id"
			   :parse-vars (list :id #'parse-integer))
  (if (user-can-moderate (ensure-auth *current-user*))
      (let ((msg (get-message id)))
	(setf (message-visible msg) nil)
	(ensure-connection 
	  (update-dao msg))
	(restas:redirect 'message-view :id id))
      (restas:redirect 'access-denied)))


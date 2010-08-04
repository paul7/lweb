(in-package :lweb)

(restas:define-route message-view (":id"
				   :parse-vars (list :id #'parse-integer))
  (let ((msg (get-message id)))
    (if msg
	(render :message (:login :thread :user) msg)
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
	(user (get-current-user)))
    (if (user-can-post user)
	(if (message-post-check :parent-id parent
				:header header
				:text text)
	    (restas:redirect 'message-view 
			     :id (message-id
				  (make-message :parent-id parent
						:header header
						:text text
						:visible (user-can-post-postmoderated user)
						:author-id (user-id user))))
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

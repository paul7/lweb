(in-package :lweb)

(restas:define-route message-view (":id"
				   :parse-vars (list :id #'parse-integer))
  (let ((msg (get-message id)))
    (if msg
	(render :message (:login :thread :user) msg)
	hunchentoot:+http-not-found+)))

(restas:define-route message-post (":parent"
				   :method :post
				   :requirement #'(lambda ()
						    (hunchentoot:post-parameter "send"))
				   :parse-vars (list :parent #'parse-integer))
  (let ((header (hunchentoot:post-parameter "header"))
	(text (hunchentoot:post-parameter "text")))
    (if (message-post-check :parent-id parent
			    :header header
			    :text text)
	(restas:redirect 'message-view 
			 :id (message-id
			      (make-message :parent-id parent
					    :header header
					    :text text
					    :author-id (user-id (get-current-user)))))
	(list :error "empty topic"
	      :return parent))))
	
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

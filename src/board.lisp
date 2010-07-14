(in-package :lweb)

(defun get-current-user-id ()
  (parse-integer (or (hunchentoot:cookie-in "uid") "0") :junk-allowed t))

(defun message-login (msg)
  (restas:genurl 'login-form :id (message-id msg)))

(defun message-url (msg)
  (restas:genurl 'message-view :id (message-id msg)))

(defun message-children (msg)
  (ensure-connection
    (let ((children (select-dao 'message 
				(:= 'parent-id (message-id msg)))))
      (mapcar #'(lambda (child)
		  (render :message (:children :url) child))
	      children))))

(defun message-thread (msg)
  (render :message (:children :url) 
	  (get-message (message-root-id* msg))))

(defun message-user (msg)
  (declare (ignore msg))
  (get-user (get-current-user-id)))

(restas:define-route message-view (":id"
				   :parse-vars (list :id #'parse-integer))
  (let ((msg (get-message id)))
    (if msg
	(render :message (:login :thread :user) msg)
	hunchentoot:+http-not-found+)))

(defun message-post-check (&key parent-id header text)
  (declare (ignore text parent-id))
  (plusp (length header)))

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
					    :author-id (get-current-user-id))))
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

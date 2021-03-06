(in-package #:lweb)

(setf *default-render-method*
      #'(lambda (obj)
	  (closure-template.standard:xhtml-strict-frame
	   (list :title (getf obj :title)
		 :body (lweb.view:main-view 
			(list 
			 :login (restas:genurl 'login-form 
                                               :return (hunchentoot:url-encode (hunchentoot:request-uri*)))
			 :impl (list :type (lisp-implementation-type)
				     :version (lisp-implementation-version))
			 :index (restas:genurl 'message-list)
			 :body (restas:render-object 
				(find-package ':lweb.view)
				obj)))))))

(restas:define-route main ("")
  (restas:redirect 'message-list))

(restas:define-route message-view (":id"
				   :parse-vars (list :id #'parse-integer))
  (if (zerop id)
      (restas:redirect 'message-list)
      (let ((msg (get-message* id)))
	(if msg
	    (if (render-visible* msg)
		(render (:message-default 
			 :thread 
			 :user
			 :writable
			 :posturl 
			 :index
			 :around) msg)
		(restas:redirect 'access-denied))
	    hunchentoot:+http-not-found+))))

(restas:define-route message-list ("index")
  (let ((messages (ensure-connection 
		    (mapcar #'get-message* (get-root-message-ids))))
	(user (ensure-auth *current-user*)))
    (list :messages (mapcar #'render-thread 
			    messages)
	  :writable (user-can-start-threads user)
	  :user (render (:user-default) user)
	  :posturl (root-posturl))))

(restas:define-route message-list-around ("index/:id"
					  :parse-vars (list :id #'parse-integer))
  (let ((messages (ensure-connection 
		    (mapcar #'get-message* 
			    (get-root-message-ids :around id))))
	(user (ensure-auth *current-user*)))
    (list :messages (mapcar #'render-thread messages)
	  :writable (user-can-start-threads user)
	  :user (render (:user-default) user)
	  :posturl (root-posturl))))

(restas:define-route start-thread ("new"
				   :method :post
				   :render-method #'lweb.view:message-post 
				   :requirement #'(lambda ()
						    (hunchentoot:post-parameter "send")))
  (let ((header (hunchentoot:post-parameter "header"))
	(text (hunchentoot:post-parameter "text"))
	(user (ensure-auth 
		*current-user*)))
    (if (user-can-start-threads user)
	(if (message-post-check :parent-id 0
				:header header
				:text text)
	    (progn
	      (make-message :parent-id 0
			    :header header
			    :text text
			    :visible t
			    :author-id (user-id user))
	      (restas:redirect 'message-list))
	    (list :error "empty topic"
		  :return (restas:genurl 'message-list)))
	(restas:redirect 'access-denied))))

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
	
(restas:define-route login-form ("login/:return"
				 :render-method #'lweb.view:login-form)
  (declare (ignore return)))

(restas:define-route login-as-uid ("login/:return"
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
  (restas:redirect return))

(define-message-action show
  (setf (message-visible message) t)
  (ensure-connection 
    (update-dao message)))

(define-message-action hide
  (ensure-connection
    (map-subthread #'(lambda (msg)
		       (setf (message-visible msg) nil)
		       (update-dao msg))
		   message)))
		       
(define-message-action erase
  (ensure-connection
    (map-subthread #'delete-dao
		   message)))

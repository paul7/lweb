(in-package :lweb)

(defun get-current-user ()
  (let* ((cookie-id (parse-integer 
		     (or (hunchentoot:cookie-in "uid") "1") 
		     :junk-allowed t))
	 (user (get-user cookie-id)))
    (or user (user-anonymous))))

(defun message-login (msg)
  (restas:genurl 'login-form :id (message-id msg)))

(defun message-url (msg)
  (restas:genurl 'message-view :id (message-id msg)))

(defun split-on (predicate list)
  (let ((if-true nil)
	(if-false nil))
    (mapc #'(lambda (each)
	      (if (funcall predicate each)
		  (push each if-true)
		  (push each if-false))) 
	  list)
    (values (nreverse if-true)
	    (nreverse if-false))))

(defun build-tree (msg)
  (let* ((root-id (message-root-id* msg))
	 (root (get-message root-id))
	 (elements (cons root (ensure-connection 
				(select-dao 'message (:= 'root-id root-id)))))
	 (parents (copy-seq elements)))
    (mapc #'(lambda (each)
	      (let ((id (message-id each)))
		(multiple-value-bind (ours theirs)
		    (split-on #'(lambda (each)
				  (= (message-parent-id each) id))
			      elements)
		  (setf (message-children~ each) ours)
		  (setf elements theirs))))
	  parents)
    root))

(defun message-children (msg)
  (let ((children (message-children~ msg)))
    (mapcar #'(lambda (child)
		(render :message (:children :url) child))
	    children)))

(defun message-thread (msg)
  (render :message (:children :url) 
	  (build-tree msg)))

(defun message-user (msg)
  (declare (ignore msg))
  (render-default (get-current-user)))

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

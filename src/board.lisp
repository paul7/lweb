(in-package :lweb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-option-function (prefix detail)
    (symb prefix 'with- detail)))

(defmacro defoption (prefix detail (obj &rest args) &body code)
  `(defun ,(make-option-function prefix detail) (,obj ,@args)
     (list* ,detail (progn ,@code)
	    ,obj)))

(defmacro with-options (prefix (&rest details) &body code)
  (if (null details)
      `(progn ,@code)
      (let* ((spec (car details))
	     (simple (atom spec))
	     (fun (if simple
		      spec
		      (car spec)))
	     (aux-args (unless simple
			 (cdr spec))))
	`(,(make-option-function prefix fun)
	   (with-options ,prefix (,@(cdr details))
	     ,@code)
	   ,@aux-args))))

(defoption :message/ :author (msg)
  (render-user (message-author-id msg)))

(defoption :message/ :url (msg)
  (restas:genurl 'message-view :id (message-id msg)))

(defoption :message/ :children (msg)
  (mapcar #'render-thread 
	  (message-children-ids msg)))

(defoption :message/ :thread (msg)
  (render-thread (message-root-id msg)))

(defoption :message/ :user (msg id)
  (render-user id))

(defun render-user (id)
  (get-user id))

(defun render-thread (id)
  (let ((root (get-message id)))
    (if root
	(with-options :message/ (:children :url :author)
	  root)
	hunchentoot:+http-not-found+)))

(defun render-message (id uid)
  (let ((msg (get-message id)))
    (if msg
	(list* :login (restas:genurl 'login-form :id id)
	       (with-options :message/ (:thread :children :author (:user uid))
		 msg))
	hunchentoot:+http-not-found+)))

(defun get-current-user-id ()
  (parse-integer (or (hunchentoot:cookie-in "uid") "0") :junk-allowed t))

(restas:define-route message-view (":id"
				   :parse-vars (list :id #'parse-integer))
  (render-message id (get-current-user-id)))

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
			 :id (add-message :parent-id parent
					  :header header
					  :text text
					  :author-id (get-current-user-id)))
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

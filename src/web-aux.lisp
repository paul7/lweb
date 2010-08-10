(in-package :lweb)

(defparameter *current-user* nil)

(defparameter *current-id* nil)

(defparameter *current-message* nil)

(defparameter *current-thread* nil)

(defun get-current-user ()
  (let* ((cookie-id (parse-integer 
		     (or (hunchentoot:cookie-in "uid") "1") 
		     :junk-allowed t))
	 (user (get-user cookie-id)))
    (or user (user-anonymous))))

(defmacro ensure-auth (&body body)
  `(let ((*current-user* *current-user*))
     (unless *current-user*
       (setf *current-user* (get-current-user)))
     ,@body))

(defmacro using-id (id &body body)
  `(let ((*current-id* ,id))
     ,@body))

(defmacro ensure-message (&body body)
  `(let ((*current-message* *current-message*))
     (unless *current-message*
       (setf *current-message* (get-message *current-id*)))
     ,@body))

(defun get-current-thread ()
  (build-tree *current-message*))

(defmacro ensure-thread (&body body)
  `(let ((*current-thread* *current-thread*))
     (unless *current-thread*
       (setf *current-thread* (ensure-message
				(get-current-thread))))
     ,@body))

(defmacro define-moderatorial (object name &body body)
  (let ((route (concatenate 'string
			    (string-downcase (symbol-name name))
			    "/:id/:return"))
	(ensure-macro (symb 'ensure- object)))
    `(restas:define-route ,name (,route
				 :parse-vars (list :id #'parse-integer))
       (using-id id
	 (if (user-can-moderate (ensure-auth *current-user*))
	     (,ensure-macro 
	      ,@body
	      (restas:redirect return))
	     (restas:redirect 'access-denied))))))

(defun message-login (msg)
  (restas:genurl 'login-form :id (message-id msg)))

(defun message-url (msg)
  (restas:genurl 'message-view :id (message-id msg)))

(defun message-moderatorial (msg)
  (let* ((id (message-id msg))
	 (return-self (hunchentoot:url-encode
		       (restas:genurl 'message-view :id id)))
	 (return-parent (hunchentoot:url-encode 
			 (restas:genurl 'message-view :id (message-parent-id msg)))))
    (if (user-can-moderate (ensure-auth *current-user*))
	(nconc
	 (list (if (message-visible msg)
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

(defun message-children (msg)
  (let ((children (message-children~ msg)))
    (mapcar #'(lambda (child)
		(render :message (:children :url :moderatorial) child))
	    children)))

(defun message-thread (msg)
  (using-id (message-id msg) 
    (render :message (:children :url :moderatorial) 
	    (ensure-thread
	      *current-thread*))))
(defun message-user (msg)
  (declare (ignore msg))
  (ensure-auth
    (render-default *current-user*)))

(defun message-writable (msg)
  (declare (ignore msg))
  (ensure-auth
    (user-can-post *current-user*)))

(defun message-posturl (msg)
  (restas:genurl 'message-post :parent (message-id msg)))

(defun root-posturl ()
  (restas:genurl 'start-thread :parent 0))

(defun message-visible* (msg)
  (or (message-visible msg)
      (ensure-auth
	(user-can-moderate *current-user*))))

(defun build-tree (msg)
  (let* ((root-id (message-root-id* msg))
	 (root (get-message root-id))
	 (elements (ensure-auth
		     (remove-if-not #'message-visible*
				    (cons root 
					  (ensure-connection 
					    (select-dao 'message 
							(:= 'root-id root-id)
							'id))))))
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

(defun message-post-check (&key parent-id header text)
  (declare (ignore text parent-id))
  (plusp (length header)))


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

(defmethod render-user ((object t))
  (ensure-auth
   (render (:user-default) *current-user*)))

(defmethod render-writable ((object t))
  (ensure-auth
    (user-can-post *current-user*)))

(defun root-posturl ()
  (restas:genurl 'start-thread :parent 0))

(defun render-visible* (msg)
  (or (render-visible msg)
      (ensure-auth
	(user-can-moderate *current-user*))))

(defun build-tree (msg-id)
  (let ((msg (get-message msg-id)))
    (if msg
	(let* ((root-id (render-root-id* msg))
	       (root (get-message root-id))
	       (msg-in-tree nil)
	       (elements 
		(ensure-auth
		  (remove-if-not #'render-visible*
				 (cons root 
				       (ensure-connection 
					 (if *reverse-order* 
					     (make-instances 
					      *message-class* 
					      (db-messages-in-thread/reverse root-id))
					     (make-instances 
					      *message-class* 
					      (db-messages-in-thread root-id))))))))
	       (parents (copy-seq elements)))
	  (mapc #'(lambda (each)
		    (let ((id (render-id each)))
		      (if (= id msg-id)
			  (setf msg-in-tree each))
		      (multiple-value-bind (ours theirs)
			  (split-on #'(lambda (each)
					(= (render-parent-id each) id))
				    elements)
			(setf (message-children~ each) ours)
			(setf elements theirs))))
		parents)
	  (values root
		  msg-in-tree)))))

(defun message-post-check (&key parent-id header text)
  (declare (ignore text parent-id))
  (plusp (length header)))


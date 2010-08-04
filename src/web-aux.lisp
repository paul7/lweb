(in-package :lweb)

(defun message-login (msg)
  (restas:genurl 'login-form :id (message-id msg)))

(defun message-url (msg)
  (restas:genurl 'message-view :id (message-id msg)))

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

(defun get-current-user ()
  (let* ((cookie-id (parse-integer 
		     (or (hunchentoot:cookie-in "uid") "1") 
		     :junk-allowed t))
	 (user (get-user cookie-id)))
    (or user (user-anonymous))))

(defun message-visible* (msg)
  (or (message-visible msg)
      (user-can-moderate (get-current-user))))

(defun build-tree (msg)
  (let* ((root-id (message-root-id* msg))
	 (root (get-message root-id))
	 (elements (remove-if-not #'message-visible*
				  (cons root (ensure-connection 
					       (select-dao 'message (:= 'root-id root-id))))))
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


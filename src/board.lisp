(in-package :lweb)

(defvar *messages* (make-hash-table))
(defvar *last-message-id* 0)

(defun clear-messages ()
  (clrhash *messages*)
  (setf *last-message-id* 0))

(defun add-message (&key 
		    (text "Hello world")
		    (author-id 1)
		    (header "Hello")
		    (visible t)
		    (parent-id nil))
  (let* ((id (incf *last-message-id*))
	 (root-id (if parent-id
		      (getf (get-message parent-id) :root-id)
		      id))
	 (message (list :author-id author-id
			:id id
			:header header
			:text text
			:visible visible
			:children-ids nil
			:root-id root-id)))
    (setf (gethash id *messages*) message)
    (if parent-id
	(push id (getf (gethash parent-id *messages*) :children-ids)))
    id))

(defun get-message (id)
  (gethash id *messages*))
  
(defun get-user (id)
  (list :id id
	:name "anonymous"))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro with-detail (prefix detail (obj &rest args) &body code)
  (macrolet ((make-fun-name (prefix detail)
		   `(symb ,prefix 'with- ,detail)))
    `(defun ,(make-fun-name prefix detail) (,obj ,@args)
       (list* ,detail (progn ,@code)
	      ,obj))))

(with-detail :message/ :author (msg)
  (render-user (getf msg :author-id)))

(with-detail :message/ :url (msg)
   (restas:genurl 'message-view :id (getf msg :id)))

(with-detail :message/ :children (msg)
  (mapcar #'render-thread 
	  (getf msg :children-ids)))

(with-detail :message/ :thread (msg)
  (render-thread (getf msg :root-id)))

(defun render-user (id)
  (get-user id))

(defun render-thread (id)
  (let ((root (get-message id)))
    (if root
	(message/with-children
	 (message/with-url
	  (message/with-author root))))))

(defun render-message (id)
  (let ((msg (get-message id)))
    (if msg
	(message/with-thread
	 (message/with-children
	  (message/with-author msg)))
	hunchentoot:+http-not-found+)))

(restas:define-route message-view (":id"
				   :parse-vars (list :id #'parse-integer))
  (render-message id))


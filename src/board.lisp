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
      `(,(make-option-function prefix (car details))
	 (with-options ,prefix (,@(cdr details))
	   ,@code))))

(defoption :message/ :author (msg)
  (render-user (message-author-id msg)))

(defoption :message/ :url (msg)
   (restas:genurl 'message-view :id (message-id msg)))

(defoption :message/ :children (msg)
  (mapcar #'render-thread 
	  (message-children-ids msg)))

(defoption :message/ :thread (msg)
  (render-thread (message-root-id msg)))

(defun render-user (id)
  (get-user id))

(defun render-thread (id)
  (let ((root (get-message id)))
    (if root
	(with-options :message/ (:children :url :author)
	  root)
	hunchentoot:+http-not-found+)))

(defun render-message (id)
  (let ((msg (get-message id)))
    (if msg
	(with-options :message/ (:thread :children :author)
	  msg)
	hunchentoot:+http-not-found+)))

(restas:define-route message-view (":id"
				   :parse-vars (list :id #'parse-integer))
  (render-message id))


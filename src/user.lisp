(in-package :lweb)

(defclass user ()
  ((id :col-type serial
       :accessor user-id)
   (nick :col-type text
	 :initarg :nick
	 :accessor user-nick))
  (:keys id)
  (:metaclass dao-class))

(defmake user)

(defclear user)

(defget user)

(defmethod render-default ((object user))
  (build-render-list :user (:id :nick) 
		     object))

(defun user-anonymous ()
  (get-user 1))

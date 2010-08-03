(in-package :lweb)

(defclass user ()
  ((id    :col-type serial
          :accessor user-id)
   (nick  :col-type text
	  :initarg :nick
	  :accessor user-nick)
   (roles :col-type integer
	  :initarg :roles
	  :initform 0
	  :accessor user-roles))
  (:keys id)
  (:metaclass dao-class))

(defmacro defrole (role)
  (let ((guser (gensym))
	(ggranted (gensym))
	(constant (symb '+user-can- role '+))
	(accessor (make-option-function 'user-can role)))
    `(progn
       (defun ,accessor (,guser)
	 (not (zerop (logand ,constant
			     (user-roles ,guser)))))
       (defun (setf ,accessor) (,ggranted ,guser)
	 (setf (user-roles ,guser)
	       (logior (user-roles ,guser)
		       (if ,ggranted 
			   ,constant
			   0)))
	 ,ggranted))))

(defconstant +user-can-post-premoderated+  1)
(defconstant +user-can-post-postmoderated+ 2)
(defconstant +user-can-moderate+           4)
(defconstant +user-can-start-threads+      8)

(defrole post-premoderated)
(defrole post-postmoderated)
(defrole moderate)
(defrole start-threads)

(defun user-can-post (user)
  (or (user-can-post-premoderated user)
      (user-can-post-postmoderated user)))

(defmethod initialize-instance :after ((user user) 
				       &key (post-premoderated t)
				       post-postmoderated
				       moderate 
				       start-threads)
  (setf (user-can-post-premoderated user) post-premoderated)
  (setf (user-can-post-postmoderated user) post-postmoderated)
  (setf (user-can-moderate user) moderate)
  (setf (user-can-start-threads user) start-threads))

(defmake user)

(defclear user)

(defget user)

(defun make-owner (&key nick)
  (make-user :nick nick
	     :post-premoderated  t
	     :post-postmoderated t
	     :moderate           t
	     :start-threads      t))

(defun make-anonymous (&key (nick "anonymous"))
  (make-user :nick nick
	     :post-premoderated  t
	     :post-postmoderated nil
	     :moderate           nil
	     :start-threads      nil))

(defmethod render-default ((object user))
  (build-render-list :user (:id :nick) 
		     object))

(defun user-anonymous ()
  (get-user 1))

(in-package #:lweb)

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
  (let ((constant (symbolicate '+user-can- role '+))
	(accessor (symbolicate 'user-can- role)))
    `(progn
       (defun ,accessor (user)
	 (not (zerop (logand ,constant
			     (user-roles user)))))
       (defun (setf ,accessor) (granted user)
	 (setf (user-roles user)
	       (if granted
		   (logior (user-roles user) ,constant)
		   (logand (user-roles user) (lognot ,constant))))
	 granted))))

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

(defun (setf user-can-post) (granted user)
  (if granted
      (setf (user-can-post-premoderated user) t)
      (setf (user-can-post-premoderated user) nil
	    (user-can-post-postmoderated user) nil)))

(defmethod initialize-instance :after ((user user) 
				       &key (post-premoderated t premod-p)
				       (post-postmoderated nil postmod-p)
				       (moderate nil mod-p)
				       (start-threads nil start-p))
  (if premod-p
      (setf (user-can-post-premoderated user) post-premoderated))
  (if postmod-p
      (setf (user-can-post-postmoderated user) post-postmoderated))
  (if mod-p
      (setf (user-can-moderate user) moderate))
  (if start-p
      (setf (user-can-start-threads user) start-threads)))

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

(define-class-options user
  (:id   (user-id user))
  (:nick (user-nick user)))

(define-option-group (:user-default)
  :id
  :nick)

(defun user-anonymous ()
  (get-user 1))

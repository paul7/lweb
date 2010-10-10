(in-package #:lweb)

(defparameter *db-spec* '("lispdb" "lisp" "lisp" "localhost" :pooled-p t))

(defparameter *message-class* 'message)

(defparameter *user-class* 'user)

(defmacro ensure-connection (&body body)
  `(if *database*
       (progn ,@body)
       (with-connection *db-spec*
	 ,@body)))

(defmacro defmake (class)
  `(defun ,(symbolicate 'make- class) (&rest args)
     (let ((msg (apply #'make-instance ',class args)))
       (ensure-connection
	 (insert-dao msg)))))

(defmacro defclear (class)
  `(defun ,(symbolicate 'clear- class) ()
       (ensure-connection 
	 (execute (:delete-from ',class))
	 (values))))

(defmacro defget (class)
  `(defun ,(symbolicate 'get- class) (id &key (class ,(symb '* class '-class*)))
     (ensure-connection
       (car (select-dao class (:= 'id id))))))

(defun create-table-for-class (class)
  (ensure-connection
    (execute (dao-table-definition class))
    (values)))

(defun drop-table-for-class (class)
  (ensure-connection 
    (execute (:drop-table class))
    (values)))

(defun install ()
  (ensure-connection 
    (mapc #'create-table-for-class '(message user))
    (make-anonymous))
  (values))

(defun uninstall ()
  (if (yes-or-no-p "This operation will purge all data. Proceed?")
      (ensure-connection
	(mapc #'drop-table-for-class '(message user))))
  (values))
  

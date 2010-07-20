(in-package :lweb)

(defparameter *db-spec* '("lispdb" "lisp" "lisp" "localhost"))

(defmacro ensure-connection (&body body)
  `(if *database*
       (progn ,@body)
       (with-connection *db-spec*
	 ,@body)))

(defmacro defmake (class)
  `(defun ,(symb 'make- class) (&rest args)
     (let ((msg (apply #'make-instance ',class args)))
       (ensure-connection
	 (insert-dao msg)))))

(defmacro defclear (class)
  `(defun ,(symb 'clear- class) ()
       (ensure-connection 
	 (execute (:delete-from ',class))
	 (values))))

(defmacro defget (class)
  `(defun ,(symb 'get- class) (id)
     (ensure-connection
       (car (select-dao ',class (:= 'id id))))))

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
    (make-user :nick "anonymous")
    (values)))

(defun uninstall ()
  (ensure-connection
    (mapc #'drop-table-for-class '(message user))
    (values)))
  

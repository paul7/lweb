(in-package :lweb)

(defparameter *db-spec* '("lispdb" "lisp" "lisp" "localhost"))

(defmacro ensure-connection (&body body)
  `(if *database*
       (progn ,@body)
       (with-connection *db-spec*
	 ,@body)))

(defun install ()
  (ensure-connection
    (execute (dao-table-definition 'message))
    (values)))

(defun uninstall ()
  (ensure-connection
    (execute (:drop-table 'message))
    (values)))
  

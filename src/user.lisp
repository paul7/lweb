(in-package :lweb)

(defun get-user (id)
  (if (zerop id)
      (list :id id
	    :name "anonymous")
      (list :id id
	    :name (format nil "user-~a" id))))

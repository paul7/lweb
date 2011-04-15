(in-package #:lweb)

(defparameter *developer-mode* nil)

(defmacro with-developer-mode (&body body)
  `(let (#+sbcl (sb-ext:*evaluator-mode* 
		 (if *developer-mode* 
		     :interpret 
		     sb-ext:*evaluator-mode*)))
     ,@body))

(defun split-on (predicate list)
  (let ((if-true nil)
	(if-false nil))
    (mapc #'(lambda (each)
	      (if (funcall predicate each)
		  (push each if-true)
		  (push each if-false))) 
	  list)
    (values (nreverse if-true)
	    (nreverse if-false))))

(defun nsplit-on (predicate list)
  (let ((if-true (cons nil nil))
	(if-false (cons nil nil)))
    (let ((true-ptr if-true)
	  (false-ptr if-false))
      (mapl #'(lambda (each)
		(if (funcall predicate (car each))
		    (setf true-ptr (setf (cdr true-ptr) each))
		    (setf false-ptr (setf (cdr false-ptr) each))))
	    list)
      (values (cdr if-true)
	      (cdr if-false)))))


(in-package #:lweb)

(defgeneric render-option (object option args))

(defmacro define-option ((class name &rest args) &body body)
  (with-gensyms (goption gargs)
    `(defmethod render-option ((,class ,class)
			       (,goption (eql ,name))
			       ,gargs)
       (destructuring-bind ,args ,gargs
	 (list ,name
	       (progn ,@body))))))

(defmacro define-class-options (class &body options)
  `(progn
     ,@(iter 
	(for (spec . body) in options)
	(destructuring-bind (name &rest args) (ensure-list spec)
	  (collect `(define-option (,class ,name ,@args)
		      ,@body))))))

(defmacro define-option-group ((name &rest args) &body contract)
  (with-gensyms (gobject goption gargs)
    `(defmethod render-option ((,gobject t)
			       (,goption (eql ,name))
			       ,gargs)
       (destructuring-bind ,args ,gargs
	 (render (,@contract) ,gobject)))))

(defmacro render ((&rest contract) object)
  (with-gensyms (gobject)
    `(let ((,gobject ,object))
       (nconc 
	,@(iter 
	   (for spec in contract)
	   (destructuring-bind (name &rest args) (ensure-list spec)
	     (collect `(render-option 
			,gobject 
			,name 
			(list ,@args)))))))))

;;;; test

(defclass test ()
  ((a :initform 5 
      :accessor test-a)
   (b :initform 6
      :accessor test-b)))

(define-class-options test
  (:a (test-a test))
  (:b (test-b test))
  ((:a+y y) (+ y y (test-a test))))

(define-option-group (:default2 y)
  :a
  :b
  (:a+y y))

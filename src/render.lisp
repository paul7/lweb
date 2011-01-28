(in-package #:lweb)

(defmacro define-option-group (name/args &body contract)
  (with-gensyms (gobject gargs)
    (destructuring-bind (name &rest args) (ensure-list name/args)
      `(progn
	 (defmethod ,(symbolicate 'render- name) 
	     ((,gobject t) &rest ,gargs)
	   (destructuring-bind ,args ,gargs
	     (values
	      (render (,@contract) ,gobject)
	      t)))))))

(defmacro render ((&rest contract) object)
  (with-gensyms (gobject gresult gsplice)
    `(let ((,gobject ,object))
       (nconc 
	,@(iter 
	   (for spec in contract)
	   (destructuring-bind (name &rest args) (ensure-list spec)
	     (collect 
		 `(multiple-value-bind (,gresult ,gsplice)
		      (apply #',(symbolicate 'render- name)
			     ,gobject 
			     (list ,@args))
		    (if ,gsplice
			,gresult
			(list ,name ,gresult))))))))))

;;;; test

(defclass test ()
  ((a :initform 5 
      :accessor test-a)
   (b :initform 6
      :accessor test-b)))

(defmethod render-a ((x test))
  (test-a x))

(defmethod render-b ((x test))
  (test-b x))

(defmethod render-a+y ((x test) y)
  (+ y y (render-a x)))

(define-option-group :default
  :a
  :b
  (:a+y 2))

(define-option-group (:default2 y)
  :a
  :b
  (:a+y y))

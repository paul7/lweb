(in-package :lweb)

(defparameter *result* 0)

(restas:define-route experiment ("experiment")
   (list :header "Hello"
	 :result (incf *result*)
	 :ololo (restas:genurl 'ololo)))

(restas:define-route ololo ("ololo"))

(defun render-list (list)
  (if (atom list)
      (list :leaf list)
      (list :children (list (render-list (car list))
			    (render-list (cdr list))))))
	    
(restas:define-route tree-view ("tree")
  (render-list '((1 2 3 4) (5 6 7) (9 . 10))))

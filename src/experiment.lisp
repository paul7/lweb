(in-package :lweb)

(defparameter *result* 0)

(setf *default-render-method*
      (lambda (obj)
        (closure-template.standard:xhtml-strict-frame
         (list :title (getf obj :title)
               :body (restas:render-object (find-package ':lweb.experiment.view)
                                           obj)))))

(restas:define-route main ("")
  (restas:redirect 'experiment))

(restas:define-route experiment ("experiment")
   (list :header "Hello"
	 :result (incf *result*)
	 :ololo (restas:genurl 'ololo)))

(restas:define-route ololo ("ololo"))

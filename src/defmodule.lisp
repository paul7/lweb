(restas:define-module :lweb
  (:use :cl :postmodern)
  (:export))

(in-package :lweb)

(defun recompile-templates ()
  (closure-template:compile-template :common-lisp-backend
				     (merge-pathnames "src/experiment.tmpl"
						      (asdf:component-pathname (asdf:find-system '#:lweb))))

  (closure-template:compile-template :common-lisp-backend
				     (merge-pathnames "src/board.tmpl"
						      (asdf:component-pathname (asdf:find-system '#:lweb)))))

(recompile-templates)

(setf *default-render-method*
      (lambda (obj)
        (closure-template.standard:xhtml-strict-frame
         (list :title (getf obj :title)
               :body (restas:render-object (find-package ':lweb.view)
                                           obj)))))

(restas:define-route main ("")
  (restas:redirect 'tree-view))

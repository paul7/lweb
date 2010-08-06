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
						      (asdf:component-pathname (asdf:find-system '#:lweb))))
  (values))

(recompile-templates)


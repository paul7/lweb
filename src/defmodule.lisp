(restas:define-module :lweb
  (:use :cl)
  (:export))

(in-package :lweb)

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/experiment.tmpl"
                                                    (asdf:component-pathname (asdf:find-system '#:lweb))))

(restas:define-module #:lweb
    (:use #:cl #:postmodern #:iterate)
  (:export message-id 
	   message-text 
	   message-header 
	   message-visible 
	   message-parent-id
	   message-root-id 
	   message-author
	   message-children~
	   message-thread~
	   render-default))

(in-package #:lweb)

(defun recompile-templates ()
  (closure-template:compile-template :common-lisp-backend
				     (merge-pathnames "src/board.tmpl"
						      (asdf:component-pathname (asdf:find-system '#:lweb))))
  (values))

(recompile-templates)


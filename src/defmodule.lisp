(restas:define-module #:lweb
    (:use #:cl #:postmodern #:iterate)
  (:export #:message-id 
	   #:message-text 
	   #:message-header 
	   #:message-visible 
	   #:message-parent-id
	   #:message-root-id 
	   #:message-author
	   #:message-children~
	   #:message-thread~
	   #:render-default
	   #:build-render-list
	   #:*message-class*
	   #:*index-limit*
	   #:*reverse-order*))

(in-package #:lweb)

(defparameter *index-limit* 10)

(defparameter *reverse-order* nil)

(defun recompile-templates ()
  (closure-template:compile-template :common-lisp-backend
				     (merge-pathnames "src/board.tmpl"
						      (asdf:component-pathname (asdf:find-system '#:lweb))))
  (values))

(recompile-templates)


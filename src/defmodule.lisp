(restas:define-module #:lweb
    (:use #:cl #:postmodern #:iterate #:alexandria)
  (:export #:message-mixin
	   #:render-id
	   #:render-text
	   #:render-header
	   #:render-visible
	   #:render-parent-id
	   #:render-root-id
	   #:render-author
	   #:render-message-default
	   #:render
	   #:define-option-group
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


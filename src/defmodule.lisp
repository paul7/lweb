(restas:define-module #:lweb
    (:use #:cl #:postmodern #:iterate #:alexandria)
  (:export #:message-mixin
	   #:db-storage
	   #:id-thread-messages
	   #:root-ids
	   #:render-id
	   #:render-text
	   #:render-header
	   #:render-visible
	   #:render-parent-id
	   #:render-root-id
	   #:render-author
	   #:render-message-default
	   #:render
	   #:ensure-connection
	   #:ensure-auth
	   #:define-option-group
	   #:*message-class*
	   #:*index-limit*
	   #:*reverse-order*))

(in-package #:lweb)

(defparameter *index-limit* 10)

(defparameter *reverse-order* nil)

(defparameter *developer-mode* nil)

(defun recompile-templates ()
  (let ((sb-ext:*evaluator-mode* 
	 (if *developer-mode* 
	     :interpret 
	     sb-ext:*evaluator-mode*)))
    (closure-template:compile-template :common-lisp-backend
				       (merge-pathnames "src/board.tmpl"
							(asdf:component-pathname (asdf:find-system '#:lweb))))
    (values)))

(recompile-templates)


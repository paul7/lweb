(restas:define-module #:lweb
    (:use #:cl #:postmodern #:iterate #:alexandria)
  (:export #:message-mixin
	   #:db-storage
	   #:make-instances
	   #:id-thread-messages
	   #:root-ids
	   #:render-id
	   #:render-display-id
	   #:render-text
	   #:render-header
	   #:render-visible
	   #:render-parent-id
	   #:render-root-id
	   #:render-author
	   #:render-message-default
	   #:render
	   #:defprepared/named
	   #:with-auth
	   #:define-option-group
	   #:*message-class*
	   #:*index-limit*
	   #:*reverse-order*))

(in-package #:lweb)

(defparameter *index-limit* 10)

(defparameter *reverse-order* nil)


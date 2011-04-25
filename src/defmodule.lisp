(restas:define-module #:lweb
    (:use #:cl #:postmodern #:iterate #:alexandria)
  (:export #:message-mixin
	   #:db-storage
	   #:with-storage
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
	   #:define-option-group
	   #:with-auth
	   #:*message-class*
	   #:*index-limit*
	   #:*reverse-order*))

(in-package #:lweb)

(defparameter *index-limit* 10)

(defparameter *reverse-order* nil)


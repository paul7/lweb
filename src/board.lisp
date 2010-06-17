(in-package :lweb)

(defun get-message (id)
  (list :author-id 1
	:id id
	:header "Hello"
	:text "Hello world!"
	:visible t
	:children nil
	:root-id 1))

(defun get-user (id)
  (list :id id
	:name "anonymous"))

(defun render-user (id)
  (get-user id))

(defun render-thread (id)
  (declare (ignore id))
  nil)

(defun render-message (id)
  (let* ((msg (get-message id))
	 (author (render-user (getf msg :author-id)))
	 (thread (render-thread (getf msg :root-id))))
    (list* :author author
	   :thread thread
	   msg)))

(restas:define-route message-view (":id"
				   :parse-vars (list :id #'parse-integer))
  (render-message id))


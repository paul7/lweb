(in-package #:lweb)

(defun recompile-templates ()
  (with-developer-mode
    (closure-template:compile-template :common-lisp-backend
				       (merge-pathnames "src/board.tmpl"
							(asdf:component-pathname (asdf:find-system '#:lweb))))
    (values)))

(recompile-templates)

(defparameter *current-user* nil)

(defmacro with-test-environment (&body body)
  `(let ((*current-user* (user-anonymous)))
     (with-storage (symbolicate '*db-storage*)
       ,@body)))

(defun get-current-user ()
  (let* ((cookie-id (parse-integer 
		     (or (hunchentoot:cookie-in "uid") "1") 
		     :junk-allowed t))
	 (user (get-user cookie-id)))
    (or user (user-anonymous))))

(defmacro with-auth (&body body)
  `(if *current-user*
       (progn ,@body)
       (let ((*current-user* (get-current-user)))
         ,@body)))

(defmacro define-message-action (name &body body)
  (let ((route (concatenate 'string
			    (string-downcase (symbol-name name))
			    "/:id/:return")))
    `(restas:define-route ,name (,route
				 :parse-vars (list :id #'parse-integer))
       (if (user-can-moderate (with-auth *current-user*))
	   (let ((message (get-message id)))
	     (if message
		 (progn 
		   ,@body
		   (restas:redirect return))
		 (restas:redirect 'access-denied)))
	   (restas:redirect 'access-denied)))))

(defmethod render-user ((object t))
  (with-auth
    (render (:user-default) *current-user*)))

(defmethod render-writable ((object t))
  (with-auth
    (user-can-post *current-user*)))

(defun root-posturl ()
  (restas:genurl 'start-thread :parent 0))

(defun render-visible* (msg)
  (or (render-visible msg)
      (with-auth
	(user-can-moderate *current-user*))))

(defun message-post-check (&key parent-id header text)
  (declare (ignore text parent-id))
  (plusp (length header)))


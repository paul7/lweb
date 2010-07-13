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
						      (asdf:component-pathname (asdf:find-system '#:lweb)))))

(recompile-templates)

(setf *default-render-method*
      (lambda (obj)
        (closure-template.standard:xhtml-strict-frame
         (list :title (getf obj :title)
               :body (restas:render-object (find-package ':lweb.view)
                                           obj)))))

(restas:define-route main ("")
  (restas:redirect 'tree-view))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun make-option-function (prefix detail)
    (symb prefix '- detail)))

(defgeneric render-default (object))

(defmacro build-render-list (prefix (&rest details) object)
  (let ((gobject (gensym)))
    `(let ((,gobject ,object))
       (list ,@(mapcan #'(lambda (detail)
			   `(,detail (,(make-option-function prefix detail) ,gobject)))
		       details)))))

(defmacro render (prefix (&rest details) object)
  (let ((gobject (gensym)))
    `(let ((,gobject ,object))
       (nconc (build-render-list ,prefix ,details ,gobject)
	      (render-default ,gobject)))))

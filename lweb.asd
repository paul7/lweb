(defsystem lweb
  :depends-on (#:restas #:closure-template #:postmodern)
  :components ((:module "src"
                        :components ((:file "defmodule")
				     (:file "db"
					    :depends-on ("defmodule"))
				     (:file "message"
					    :depends-on ("db"))
				     (:file "user"
					    :depends-on ("db"))
				     (:file "experiment"
					    :depends-on ("db"))
				     (:file "board"
					    :depends-on ("message"
							 "user"))))))

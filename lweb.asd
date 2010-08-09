(defsystem lweb
  :depends-on (#:restas #:closure-template #:postmodern)
  :components ((:module "src"
                        :components ((:file "defmodule")
				     (:file "aux"
					    :depends-on ("defmodule"))
				     (:file "db"
					    :depends-on ("aux"))
				     (:file "message"
					    :depends-on ("db"))
				     (:file "user"
					    :depends-on ("db"))
				     (:file "web-aux"
					    :depends-on ("message"
							 "user"))
				     (:file "board"
					    :depends-on ("web-aux"))))))

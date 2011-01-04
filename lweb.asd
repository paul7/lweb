(defsystem #:lweb
  :depends-on (#:restas #:closure-template #:postmodern #:alexandria)
  :components ((:module "src"
                        :components ((:file "defmodule")
				     (:file "render"
					    :depends-on ("defmodule"))
				     (:file "aux"
					    :depends-on ("defmodule"))
				     (:file "db"
					    :depends-on ("aux"))
				     (:file "message"
					    :depends-on ("db"
							 "render"))
				     (:file "user"
					    :depends-on ("db"
							 "render"))
				     (:file "web-aux"
					    :depends-on ("message"
							 "user"))
				     (:file "board"
					    :depends-on ("web-aux"))))))

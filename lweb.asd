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
				     (:file "web-aux"
					    :depends-on ("db"
							 "render"))
				     (:file "message"
					    :depends-on ("web-aux"))
				     (:file "user"
					    :depends-on ("web-aux"))
				     (:file "board"
					    :depends-on ("message"
							 "user"))))))

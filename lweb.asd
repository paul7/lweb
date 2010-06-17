(defsystem lweb
  :depends-on (#:restas #:closure-template)
  :components ((:module "src"
                        :components ((:file "defmodule")
				     (:file "experiment"
					    :depends-on ("defmodule"))
				     (:file "board"
					    :depends-on ("defmodule"))))))

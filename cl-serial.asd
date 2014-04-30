(defsystem :cl-serial
  :name :cl-serial
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "packages")
               #+sbcl (:file "serial-lowlevel-sbcl")
               #+clisp (:file "serial-lowlevel-clisp")
               (:file "serial-lowlevel")
               (:file "serial"))
  :depends-on (:trivial-gray-streams))

(defsystem :cl-serial
  :name :cl-serial
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components (#+sbcl (:file "serial-lowlevel-sbcl")
                      (:file "serial"))
  :depends-on (:trivial-gray-streams))

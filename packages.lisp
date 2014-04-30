#-(or clisp sbcl)
(eval-when (:compile-toplevel :load-toplevel)
  (error "Only SBCL and clisp are supported"))

(defpackage serial-lowlevel
  (:use #:cl)
  (:export #:open-serial
           #:configure-serial
           #:serial-error))

(defpackage serial
  (:use #:cl #:trivial-gray-streams #:serial-lowlevel)
  (:export #:serial-device-input
           #:serial-device-output
           #:serial-device-io
           #:serial-device-baudrate
           #:serial-device-framesize
           #:serial-device-stopbits
           #:serial-device-parity
           #:serial-device-canon-p
           #:with-serial-device
           #:reset-old-value))

(in-package :serial-lowlevel)
(define-condition serial-error () ()
  (:documentation "Parent of all conditions associated with serial streams"))

(define-condition serial-invalid-parameter (serial-error)
  ((parameter :reader invalid-parameter
              :initarg :parameter))
  (:report (lambda (c s)
             (format s "Invalid parameter was specified: ~A"
                     (invalid-parameter c)))))

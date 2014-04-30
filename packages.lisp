#-(or (and linux clisp) sbcl)
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
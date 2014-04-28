#-sbcl
(eval-when (:compile-toplevel :load-toplevel)
  (error "Only SBCL is supported"))

(defpackage serial-lowlevel
  (:use #:cl)
  (:export #:open-serial
           #:configure-serial))
(in-package :serial-lowlevel)

(defmacro modify-bitfield (field &key disable enable)
  `(progn
     (setf ,field (logand ,field (lognot (logior ,@disable)))
           ,field (logior ,field ,@enable))))

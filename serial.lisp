(in-package :serial)

;; FIXME: Do not forget error handling
(defclass serial-device (fundamental-binary-stream)
  ((name         :reader serial-device-name
                 :initarg :name
                 :initform (error "Specify the name of a device"))
   (stream       :accessor serial-device-stream
                 :documentation "Stream associated with device")
   (fd           :accessor serial-device-fd
                 :documentation "File descriptor associated with device")
   (baudrate     :accessor serial-device-baudrate
                 :initarg :baudrate
                 :initform 9600
                 :documentation "Baudrate"
                 :type (integer 0))
   (framesize    :accessor serial-device-framesize
                 :initarg :framesize
                 :initform 8
                 :documentation "Data bits in frame"
                 :type (integer 5 8))
   (stopbits     :accessor serial-device-stopbits
                 :initarg :stopbits
                 :initform 1
                 :documentation "Number of stop bits. May be 1 or 2"
                 :type (integer 1 2))
   (parity       :accessor serial-device-parity
                 :initarg :parity
                 :initform #\N
                 :documentation "Parity check. May be #\N, #\O or #\E"
                 :type (member #\N #\O #\E))
   (canonp       :accessor serial-device-canon-p
                 :initarg :canonp
                 :initform t
                 :type boolean
                 :documentation "Canonical or raw io"))
  (:documentation "Serial device. Not to be instaniated"))

(defun configure-serial-device (device)
  (declare (type serial-device device))
  (configure-serial (serial-device-fd device)
                    (serial-device-baudrate device)
                    :canon (serial-device-canon-p device)
                    :framesize (serial-device-framesize device)
                    :stopbits (serial-device-stopbits device)
                    :parity (serial-device-parity device)))

(defmethod initialize-instance :after ((device serial-device) &rest initargs)
  (declare (ignore initargs))
  (multiple-value-bind (stream fd)
      (open-serial (serial-device-name device)
                   :input (input-stream-p device)
                   :output (output-stream-p device))
    (setf (serial-device-stream device) stream
          (serial-device-fd device) fd))
  (handler-bind
      (((or serial-error
            #+sbcl sb-posix:syscall-error)
        #'(lambda (c)
            (declare (ignore c))
            (close device))))
    (configure-serial-device device)))

(defmethod close ((device serial-device) &rest args)
  (declare (ignore args))
  (close (serial-device-stream device))
  (call-next-method))

(macrolet
    ((def-accessors-with-update (names)
       `(progn
          ,@(loop for name in names collect
                 `(defmethod (setf ,name) :around (val (device serial-device))
                             (declare (ignore val))
                             (let ((oldval (,name device)))
                               (call-next-method)
                               (restart-case
                                   (if (open-stream-p device)
                                       (configure-serial-device device))
                                 (reset-old-value ()
                                   :report "Fall back to the old value"
                                   (setf (,name device) oldval)))))))))
  
  (def-accessors-with-update (serial-device-baudrate
                              serial-device-framesize
                              serial-device-stopbits
                              serial-device-parity
                              serial-device-canon-p)))

(defmethod print-object ((device serial-device) stream)
  (print-unreadable-object (device stream :type t :identity t)
    (format stream "~d ~d~c~d"
            (serial-device-baudrate device)
            (serial-device-framesize device)
            (serial-device-parity device)
            (serial-device-stopbits device))))

(defclass serial-device-input (serial-device fundamental-binary-input-stream)
  ()
  (:documentation "Serial device open for input"))

(defclass serial-device-output (serial-device fundamental-binary-output-stream)
  ()
  (:documentation "Serial device open for output"))

(defclass serial-device-io (serial-device-output serial-device-input)
  ()
  (:documentation "Bidirectional serial device"))

(defmethod stream-read-byte ((stream serial-device-input))
  (read-byte (serial-device-stream stream)))
(defmethod stream-write-byte ((stream serial-device-output) byte)
  (write-byte byte (serial-device-stream stream)))

(defmethod stream-read-sequence ((stream serial-device-input) sequence start end &key)
  (read-sequence sequence stream :start start :end end))
(defmethod stream-write-sequence ((stream serial-device-output) sequence start end &key)
  (write-sequence sequence stream :start start :end end))

(defmacro with-serial-device ((var type &rest args) &body body)
  `(let ((,var (make-instance ',type ,@args)))
     (unwind-protect
          (progn ,@body)
       (close ,var))))

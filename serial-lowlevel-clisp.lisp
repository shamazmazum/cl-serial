(in-package :serial-lowlevel)

(eval-when (:compile-toplevel :load-toplevel)
  (require "linux"))

(define-condition termios-error (serial-error)
  ()
  (:report (lambda (c s)
             ;; We cannot get errno in clisp
             ;; so nothing to report, actually
             (declare (ignore c))
             (format s "System call error"))))

(defun open-serial% (devname input output)
  (let ((fd (unix:open devname
                       (logior unix:O_NDELAY
                               unix:O_NOCTTY
                               (cond
                                 ((and output input) unix:O_RDWR)
                                 (output unix:O_WRONLY)
                                 (input  unix:O_RDONLY)
                                 (t 0))) unix:DEFFILEMODE)))
    (if (/= fd -1) fd
        (error 'termios-error))))

(defun make-stream-from-fd (fd input output)
  (ext:make-stream fd
                   :direction (cond
                                ((and input output) :io)
                                (input  :input)
                                (output :output)) 
                   :element-type '(unsigned-byte 8)))

(defun open-serial (devname &key input output)
  (let* ((fd (open-serial% devname input output))
         (stream (make-stream-from-fd fd input output)))
    (unix:close fd)
    (multiple-value-bind (in-fd out-fd)
        (ext:stream-handles stream)
      (values
       stream
       (if input in-fd out-fd)))))

(defun tcgetattr (fd)
  (multiple-value-bind (ret attr)
      (unix:tcgetattr fd)
    (if (= ret 0) attr
        (error 'termios-error))))
(defun tcsetattr (fd attr)
  (let ((res (unix:tcsetattr fd unix:TCSANOW attr)))
    (if (/= res 0) (error 'termios-error))))
(defun cfsetispeed (speed attr)
  (unix:cfsetispeed attr speed))
(defun cfsetospeed (speed attr)
  (unix:cfsetospeed attr speed))
(defun termios-cflag (attr)
  (unix:termios-c_cflag attr))
(defun termios-lflag (attr)
  (unix:termios-c_lflag attr))
(defun termios-iflag (attr)
  (unix:termios-c_iflag attr))
(defun termios-oflag (attr)
  (unix:termios-c_oflag attr))
(defun (setf termios-cflag) (flag attr)
  (setf (unix:termios-c_cflag attr) flag))
(defun (setf termios-lflag) (flag attr)
  (setf (unix:termios-c_lflag attr) flag))
(defun (setf termios-iflag) (flag attr)
  (setf (unix:termios-c_iflag attr) flag))
(defun (setf termios-oflag) (flag attr)
  (setf (unix:termios-c_oflag attr) flag))

(defparameter *baudrates*
  `((50     . ,unix:B50)
    (75     . ,unix:B75)
    (110    . ,unix:B110)
    (200    . ,unix:B200)
    (300    . ,unix:B300)
    (600    . ,unix:B600)
    (1200   . ,unix:B1200)
    (1800   . ,unix:B1800)
    (2400   . ,unix:B2400)
    (4800   . ,unix:B4800)
    (9600   . ,unix:B9600)
    (19200  . ,unix:B19200)
    (38400  . ,unix:B38400)
    (57600  . ,unix:B57600)
    (115200 . ,unix:B115200)
    (230400 . ,unix:B230400)))

(defparameter *framesizes*
  `((5 . ,unix:CS5)
    (6 . ,unix:CS6)
    (7 . ,unix:CS7)
    (8 . ,unix:CS8)))

;; Other useful constatns
(defconstant csize unix:CSIZE)
(defconstant parenb unix:PARENB)
(defconstant parodd unix:PARODD)
(defconstant inpck unix:INPCK)
(defconstant istrip unix:ISTRIP)
(defconstant cstopb unix:CSTOPB)
(defconstant clocal unix:CLOCAL)
(defconstant cread unix:CREAD)
(defconstant icanon unix:ICANON)
(defconstant echo unix:ECHO)
(defconstant echoe unix:ECHOE)
(defconstant isig unix:ISIG)
(defconstant opost unix:OPOST)

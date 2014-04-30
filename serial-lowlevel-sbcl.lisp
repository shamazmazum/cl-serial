(in-package :serial-lowlevel)
(eval-when (:compile-toplevel :load-toplevel)
  (require 'sb-posix))

(defun open-serial% (devname input output)
  (sb-posix:open devname
                 (logior sb-posix:o-ndelay
                         sb-posix:o-noctty
                         (cond
                           ((and input output) sb-posix:o-rdwr)
                           (output             sb-posix:o-wronly)
                           (input              sb-posix:o-rdonly)
                           (t 0)))))

(defun make-stream-from-fd (fd input output)
  (sb-sys:make-fd-stream fd
                         :input input
                         :output output
                         :element-type '(unsigned-byte 8)))

(defun open-serial (devname &key input output)
  (let ((fd (open-serial% devname input output)))
    (values
     (make-stream-from-fd fd input output)
     fd)))

(defun tcgetattr (fd)
  (sb-posix:tcgetattr fd))
(defun tcsetattr (fd attr)
  (sb-posix:tcsetattr fd sb-posix:tcsanow attr))
(defun cfsetispeed (speed attr)
  (sb-posix:cfsetispeed speed attr))
(defun cfsetospeed (speed attr)
  (sb-posix:cfsetospeed speed attr))
(defun termios-cflag (attr)
  (sb-posix:termios-cflag attr))
(defun termios-lflag (attr)
  (sb-posix:termios-lflag attr))
(defun termios-iflag (attr)
  (sb-posix:termios-iflag attr))
(defun termios-oflag (attr)
  (sb-posix:termios-oflag attr))
(defun (setf termios-cflag) (flag attr)
  (setf (sb-posix:termios-cflag attr) flag))
(defun (setf termios-lflag) (flag attr)
  (setf (sb-posix:termios-lflag attr) flag))
(defun (setf termios-iflag) (flag attr)
  (setf (sb-posix:termios-iflag attr) flag))
(defun (setf termios-oflag) (flag attr)
  (setf (sb-posix:termios-oflag attr) flag))

(defparameter *baudrates*
  `((50     . ,sb-posix:B50)
    (75     . ,sb-posix:B75)
    (110    . ,sb-posix:B110)
    (200    . ,sb-posix:B200)
    (300    . ,sb-posix:B300)
    (600    . ,sb-posix:B600)
    (1200   . ,sb-posix:B1200)
    (1800   . ,sb-posix:B1800)
    (2400   . ,sb-posix:B2400)
    (4800   . ,sb-posix:B4800)
    (9600   . ,sb-posix:B9600)
    (19200  . ,sb-posix:B19200)
    (38400  . ,sb-posix:B38400)
    (57600  . ,sb-posix:B57600)
    (115200 . ,sb-posix:B115200)
    (230400 . ,sb-posix:B230400)))

(defparameter *framesizes*
  `((5 . ,sb-posix:CS5)
    (6 . ,sb-posix:CS6)
    (7 . ,sb-posix:CS7)
    (8 . ,sb-posix:CS8)))

;; Other useful constatns
(defconstant csize sb-posix:csize)
(defconstant parenb sb-posix:parenb)
(defconstant parodd sb-posix:parodd)
(defconstant inpck sb-posix:inpck)
(defconstant istrip sb-posix:istrip)
(defconstant cstopb sb-posix:cstopb)
(defconstant clocal sb-posix:clocal)
(defconstant cread sb-posix:cread)
(defconstant icanon sb-posix:icanon)
(defconstant echo sb-posix:echo)
(defconstant echoe sb-posix:echoe)
(defconstant isig sb-posix:isig)
(defconstant opost sb-posix:opost)

(defpackage serial-lowlevel
  (:use #:cl)
  (:export #:open-serial
           #:configure-serial))
(in-package :serial-lowlevel)

(eval-when (:compile-toplevel :load-toplevel)
  (require 'sb-posix))

(defun open-serial% (devname flags)
  (sb-posix:open devname
                 (logior flags sb-posix:o-ndelay sb-posix:o-noctty)))

(defun open-serial (devname &key input output element-type)
  (declare (type (member character unsigned-byte) element-type))
  (let ((fd (open-serial% devname
                          (cond
                            ((and input output) sb-posix:o-rdwr)
                             (output            sb-posix:o-wronly)
                             (input             sb-posix:o-rdonly)))))

    (values
     (sb-sys:make-fd-stream fd
                            :input  input
                            :output output
                            :element-type element-type)
     fd)))

(defun close-serial (stream)
  (close stream)) ; Just an alias

(defun configure-serial (fd baudrate &key binary (framesize 8) (stopbits 1) (parity #\N))
  (declare (type (member 5 6 7 8) framesize)
           (type (member 1 2) stopbits)
           (type (member #\N #\O #\E) parity))
  (let ((attr (sb-posix:tcgetattr fd)))
    ;; Set new baudrate
    (let ((baudrate-bits
           ;; Try this dirty hack
           (eval
            (intern
             (format nil "B~D" baudrate)
             (find-package :sb-posix)))))
      (sb-posix:cfsetispeed baudrate-bits attr)
      (sb-posix:cfsetospeed baudrate-bits attr))
    
    (with-accessors ((cflag sb-posix:termios-cflag)
                     (lflag sb-posix:termios-lflag)
                     (iflag sb-posix:termios-iflag)
                     (oflag sb-posix:termios-oflag)) attr
      ;; Set frame size
      (let ((size-bits
             (eval
              (intern
               (format nil "CS~D" framesize)
               (find-package :sb-posix)))))
      (setf cflag (logand (lognot sb-posix:csize) cflag)) ; Reset the old value
      (setf cflag (logior size-bits cflag)))
      
      ;; Set parity...
      (setf cflag (logand (lognot sb-posix:parenb) cflag))
      (setf cflag (logand (lognot sb-posix:parodd) cflag)) ; ... first reseting the old value
      (setf cflag (logior cflag
                          (if (char/= #\N parity) sb-posix:parenb 0)
                          (if (char=  #\O parity) sb-posix:parodd 0))
            iflag (if (char/= #\N parity)
                      (logior iflag sb-posix:inpck sb-posix:istrip)
                      iflag))
                          

      ;; Set additional stop bit if needed
      (setf cflag (logand (lognot sb-posix:cstopb) cflag))
      (if (= stopbits 2) (setf cflag (logior sb-posix:cstopb cflag)))

      ;; Enable normal operation
      (setf cflag (logior cflag sb-posix:clocal sb-posix:cread))

      ;; Set raw or canonical input and output
      (let ((canon-bits (logior sb-posix:icanon
                                sb-posix:echo
                                sb-posix:echoe)))
        (if binary
            (setf lflag (logand lflag
                                (lognot
                                 (logior canon-bits sb-posix:isig)))
                  oflag (logand (lognot sb-posix:opost) oflag))
            (setf lflag (logior lflag canon-bits)))))

    ;; Set new attributes
    (sb-posix:tcsetattr fd sb-posix:tcsanow attr))
  t)

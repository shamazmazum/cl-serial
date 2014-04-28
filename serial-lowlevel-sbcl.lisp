(in-package :serial-lowlevel)

(eval-when (:compile-toplevel :load-toplevel)
  (require 'sb-posix))

(defun open-serial% (devname flags)
  (sb-posix:open devname
                 (modify-bitfield flags
                                  :enable (sb-posix:o-ndelay sb-posix:o-noctty))))

(defun open-serial (devname &key input output)
  (let ((fd (open-serial% devname
                          (cond
                            ((and input output) sb-posix:o-rdwr)
                             (output            sb-posix:o-wronly)
                             (input             sb-posix:o-rdonly)))))

    (values
     (sb-sys:make-fd-stream fd
                            :input  input
                            :output output
                            :element-type '(unsigned-byte 8))
     fd)))

(defun close-serial (stream)
  (close stream)) ; Just an alias

(defun configure-serial (fd baudrate &key (canon t) (framesize 8) (stopbits 1) (parity #\N))
  (declare (type (member 5 6 7 8) framesize)
           (type (member 1 2) stopbits)
           (type (member #\N #\O #\E) parity)
           (type boolean canon))
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
      (modify-bitfield cflag
                       :disable (sb-posix:csize)
                       :enable  (size-bits)))
      
      ;; Set parity...
      (modify-bitfield cflag :disable (sb-posix:parenb sb-posix:parodd))
      (when (char/= #\N parity)
        (modify-bitfield cflag :enable (sb-posix:parenb))
        (modify-bitfield iflag :enable (sb-posix:inpck sb-posix:istrip)))
      (when (char= #\O parity)
        (modify-bitfield cflag :enable (sb-posix:parodd 0)))

      ;; Set additional stop bit if needed
      (modify-bitfield cflag :disable (sb-posix:cstopb))
      (if (= stopbits 2) (modify-bitfield cflag :enable (sb-posix:cstopb)))

      ;; Enable normal operation
      (modify-bitfield cflag
                       :enable (sb-posix:clocal sb-posix:cread))

      ;; Set raw or canonical input and output
      (let ((canon-bits (logior sb-posix:icanon
                                sb-posix:echo
                                sb-posix:echoe)))
        (modify-bitfield lflag :disable (canon-bits))
        (cond
          (canon
           (modify-bitfield lflag :disable (sb-posix:isig))
           (modify-bitfield oflag :disable (sb-posix:opost)))
          (t
           (modify-bitfield lflag :enable (canon-bits))))))

    ;; Set new attributes
    (sb-posix:tcsetattr fd sb-posix:tcsanow attr))
  t)

(in-package :serial-lowlevel)

(defmacro modify-bitfield (field &key disable enable)
  `(progn
     (setf ,field (logand ,field (lognot (logior ,@disable)))
           ,field (logior ,field ,@enable))))

;; serial-error is platform specific

(defun open-serial (devname &key input output)
  (let ((fd (open-serial% devname input output)))
    (values
     (make-stream-from-fd fd input output)
     fd)))

(defun close-serial (stream)
  (close stream)) ; Just an alias

(defun configure-serial (fd baudrate &key (canon t) (framesize 8) (stopbits 1) (parity #\N))
  (declare (type (integer 5 8) framesize)
           (type (integer 1 2) stopbits)
           (type (member #\N #\O #\E) parity)
           (type (member 50 75 110 134
                         150 200 300 600
                         1200 1800 2400 4800
                         9600 19200 38400
                         57600 115200 230400)
                 baudrate)
           (type boolean canon))
  (let ((attr (tcgetattr fd)))
    ;; Set new baudrate
    (let ((baudrate-bits (cdr (assoc baudrate *baudrates*))))
      (cfsetispeed baudrate-bits attr)
      (cfsetospeed baudrate-bits attr))
    
    (with-accessors ((cflag termios-cflag)
                     (lflag termios-lflag)
                     (iflag termios-iflag)
                     (oflag termios-oflag)) attr
      ;; Set frame size
      (let ((framesize (cdr (assoc framesize *framesizes*))))
        (modify-bitfield cflag
                         :disable (csize)
                         :enable  (framesize)))
      
      ;; Set parity...
      (modify-bitfield cflag :disable (parenb parodd))
      (when (char/= #\N parity)
        (modify-bitfield cflag :enable (parenb))
        (modify-bitfield iflag :enable (inpck istrip)))
      (when (char= #\O parity)
        (modify-bitfield cflag :enable (parodd)))

      ;; Set additional stop bit if needed
      (if (= stopbits 2)
          (modify-bitfield cflag :enable (cstopb))
          (modify-bitfield cflag :disable (cstopb)))

      ;; Enable normal operation
      (modify-bitfield cflag :enable (clocal cread))

      ;; Set raw or canonical input and output
      (let ((canon-bits (logior icanon echo echoe)))
        (cond
          (canon
           (modify-bitfield lflag :enable (canon-bits)))
          (t
           (modify-bitfield lflag :disable (canon-bits isig))
           (modify-bitfield oflag :disable (opost))))))
    
    ;; Set new attributes
    (tcsetattr fd attr))
  t)

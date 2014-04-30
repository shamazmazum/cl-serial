(in-package :serial-lowlevel)

(defmacro modify-bitfield (field &key disable enable)
  `(progn
     (setf ,field (logand ,field (lognot (logior ,@disable)))
           ,field (logior ,field ,@enable))))

(macrolet ((def-assoc-getter (name list)
              (let ((arg (gensym)))
                `(defun ,name (,arg)
                   (or (cdr (assoc ,arg ,list))
                       (error 'serial-invalid-parameter
                              :parameter ',name))))))
  (def-assoc-getter get-baudrate *baudrates*)
  (def-assoc-getter get-framesize *framesizes*))

(defun close-serial (stream)
  (close stream)) ; Just an alias

(defun configure-serial (fd baudrate &key (canon t) (framesize 8) (stopbits 1) (parity #\N))
  (declare (type (integer 0) framesize stopbits baudrate)
           (type boolean canon))
  (let ((attr (tcgetattr fd)))
    ;; Set new baudrate
    (let ((baudrate-bits (get-baudrate baudrate)))
      (cfsetispeed baudrate-bits attr)
      (cfsetospeed baudrate-bits attr))
    
    (with-accessors ((cflag termios-cflag)
                     (lflag termios-lflag)
                     (iflag termios-iflag)
                     (oflag termios-oflag)) attr
      ;; Set frame size
      (let ((framesize (get-framesize framesize)))
        (modify-bitfield cflag
                         :disable (csize)
                         :enable  (framesize)))
      
      ;; Set parity...
      (cond
        ((char= #\N parity)
         (modify-bitfield cflag :disable (parenb parodd)))
        ((char= #\E parity)
         (modify-bitfield cflag :enable (parenb) :disable (parodd)))
        ((char= #\O parity)
         (modify-bitfield cflag :enable (parenb parodd)))
        (t (error 'serial-invalid-parameter :parameter 'parity)))
      (if (char/= parity #\N) (modify-bitfield iflag :enable (inpck istrip)))

      ;; Set additional stop bit if needed
      (cond
        ((= stopbits 1)
         (modify-bitfield cflag :disable (cstopb)))
        ((= stopbits 2)
         (modify-bitfield cflag :enable (cstopb)))
        (t (error 'serial-invalid-parameter :parameter 'stopbits)))

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

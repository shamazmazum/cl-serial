cl-serial
=========

Simple library for work with serial ports as binary streams. It works currently on SBCL. CLISP support is planned.

Example of use with flexi-streams
---------------------------------

```
(with-serial-device
 (device serial-device-input
         :name "/dev/cuaa0"
         :baudrate 9600
         :stopbits 2
         :parity #\E)
 (let ((stream (make-flexi-stream device)))
   (read-line stream)))
```

Also you can configure the device later using accessors:
```
(with-serial-device
 (device serial-device-input
         :name "/dev/cuaa0")
 (setf (serial-device-framesize device) 7)
 .....)
```

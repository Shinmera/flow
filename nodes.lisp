#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flow)

(defgeneric attribute (unit attribute))
(defgeneric (setf attribute) (value unit attribute))
(defgeneric represent (unit target))
(defgeneric sever (unit))
(defgeneric connections (unit))
(defgeneric ports (node))
(defgeneric port (node name))
(defgeneric (setf port) (port node name))
(defgeneric accepting-p (connection node port))
(defgeneric connect (from to &optional connection-type &rest initargs))

(defclass unit ()
  ())

(defclass connection (unit)
  (left
   right))

(defclass directed-connection (connection)
  (direction))

(defclass port (unit)
  (node
   connections))

(defclass 1-port (port)
  ())

(defmethod accepting-p (connection node (port 1-port))
  (null (connections port)))

(defclass n-port (port)
  ())

(defmethod accepting-p (connection node (port n-port))
  T)

(defclass node (unit)
  (ports))

#+NIL
(progn
  (define-node decision ()
    (:ports (in n-port)
            (true 1-port)
            (false 1-port)))

  (define-node process ()
    (:ports (in n-port)
            (out n-port)))

  (let ((a (make-instance 'decision))
        (b (make-instance 'process)))
    (connect (port a 'true) (port b 'in))
    (connect (port a 'false) (port a 'in))
    (connect (port b 'out) (port a 'in))))

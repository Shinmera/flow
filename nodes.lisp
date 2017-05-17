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
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right)))

(defclass directed-connection (connection)
  ((direction :initarg :direction :accessor direction))
  (:default-initargs
   :direction :left->right))

(defclass node-class (standard-class)
  ())

(defmethod c2mop:validate-superclass ((class node-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass node-class))
  T)

(defmethod c2mop:validate-superclass ((class node-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class node-class) (superclass node-class))
  T)

(defclass port-definition (c2mop:standard-direct-slot-definition)
  ((port-type :initarg :port-type :accessor port-type))
  (:default-initargs
   :port-type 'c2mop:standard-effective-slot-definition))

(defclass port (c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:compute-effective-slot-definition ((class port-class) name direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (loop for direct-slot in direct-slots
          do (when (and (typep direct-slot 'port-definition)
                        (eql (c2mop:slot-definition-name direct-slot)
                             (c2mop:slot-definition-name effective-slot)))
               (change-class effective-slot (port-type direct-slot))
               (loop-finish)))
    effective-slot))

(defmethod c2mop:direct-slot-definition-class ((class port-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'port-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class port-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'c2mop:standard-effective-slot-definition))

(defclass node (unit)
  ()
  (:metaclass node-class))

(defmacro define-node (name direct-superclasses direct-slots &rest options)
  (unless (find :metaclass options :key #'first)
    (push `(:metaclass node-class) options))
  `(defclass ,name (,@direct-superclasses node)
     ,direct-slots
     ,@options))

#+NIL
(progn
  (define-node decision ()
    ((in :type n-port)
     (true :type 1-port)
     (false :type 1-port)))

  (define-node process ()
    ((in :type n-port)
     (out :type n-port)))

  (let ((a (make-instance 'decision))
        (b (make-instance 'process)))
    (connect (port a 'true) (port b 'in))
    (connect (port a 'false) (port a 'in))
    (connect (port b 'out) (port a 'in))))

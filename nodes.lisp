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
(defgeneric ports (node))
(defgeneric port (node name))
(defgeneric connections (node port))
(defgeneric (setf connections) (connections node port))
(defgeneric accepting-p (connection node port))
(defgeneric connect (from from-port to to-port &optional connection-type &rest initargs))

(defclass unit ()
  ())

(defclass connection (unit)
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right)))

(defclass directed-connection (connection)
  ((direction :initarg :direction :accessor direction))
  (:default-initargs
   :direction :left->right))

(defclass port-definition (c2mop:standard-direct-slot-definition)
  ((port-type :initarg :port-type :accessor port-type))
  (:default-initargs
   :port-type 'c2mop:standard-effective-slot-definition))

(defclass port (unit c2mop:standard-effective-slot-definition)
  ())

(defclass n-port (port)
  ())

(defmethod accepting-p (connection node (port n-port))
  T)

(defclass 1-port (port)
  ())

(defmethod accepting-p (connection node (port 1-port))
  (null (connections node port)))

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

(defmethod c2mop:compute-effective-slot-definition ((class node-class) name direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (loop for direct-slot in direct-slots
          do (when (and (typep direct-slot 'port-definition)
                        (eql (c2mop:slot-definition-name direct-slot)
                             (c2mop:slot-definition-name effective-slot)))
               (change-class effective-slot (port-type direct-slot))
               (loop-finish)))
    effective-slot))

(defmethod c2mop:direct-slot-definition-class ((class node-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'port-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class node-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'c2mop:standard-effective-slot-definition))

(defmethod ports ((node node-class))
  (remove-if-not (lambda (slot) (typep slot 'port))
                 (c2mop:class-slots node)))

(defmethod port ((node node-class) name)
  (let ((slot (find name (c2mop:class-slots node)
                    :key #'c2mop:slot-definition-name)))
    (when (typep slot 'port)
      slot)))

(defclass node (unit)
  ()
  (:metaclass node-class))

(defmacro define-node (name direct-superclasses direct-slots &rest options)
  (unless (find :metaclass options :key #'first)
    (push `(:metaclass node-class) options))
  `(defclass ,name (,@direct-superclasses node)
     ,direct-slots
     ,@options))

(defmethod ports ((node node))
  (ports (class-of node)))

(defmethod port ((node node) name)
  (port (class-of node) name))

(defmethod connections (node (port port))
  (connections node (c2mop:slot-definition-name port)))

(defmethod connections ((node node) (name symbol))
  (slot-value node name))

(defmethod (setf connections) (connections node (port port))
  (setf (connections node (c2mop:slot-definition-name port)) connections))

(defmethod (setf connections) (connections (node node) (name symbol))
  (setf (slot-value node name) connections))

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

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
(defgeneric connections (unit))
(defgeneric (setf connections) (connections port))
(defgeneric accepting-p (connection port))
(defgeneric connect (from to &optional connection-type &rest initargs))

(defclass unit ()
  ())

(defclass connection (unit)
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right)))

(defclass directed-connection (connection)
  ())

(defclass port (unit)
  ((connections :initarg :connections :initform () :accessor connections)
   (node :initarg :node :initform NIL :accessor node)))

(defclass n-port (port)
  ())

(defmethod accepting-p (connection (port n-port))
  T)

(defclass 1-port (port)
  ())

(defmethod accepting-p (connection (port 1-port))
  (null (connections port)))

(defclass port-definition ()
  ((port-type :initarg :port-type :accessor port-type))
  (:default-initargs
   :port-type NIL))

(defclass direct-port-definition (port-definition c2mop:standard-direct-slot-definition)
  ())

(defclass effective-port-definition (port-definition c2mop:standard-effective-slot-definition)
  ())

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
               (setf (port-type effective-slot) (port-type direct-slot))
               (loop-finish)))
    effective-slot))

(defmethod c2mop:direct-slot-definition-class ((class node-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-port-definition))

(defmethod c2mop:effective-slot-definition-class ((class node-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'effective-port-definition))

(defmethod ports ((node node-class))
  (loop for slot in (c2mop:class-slots node)
        when (port-type slot)
        collect (slot-value node (c2mop:slot-definition-name slot))))

(defmethod port ((node node-class) (name symbol))
  (let ((slot (find name (c2mop:class-slots node)
                    :key #'c2mop:slot-definition-name)))
    (when (port-type slot)
      (slot-value node name))))

(defclass node (unit)
  ()
  (:metaclass node-class))

(defmethod shared-initialize ((node node) initform-slots &rest initargs)
  (let ((slots (c2mop:class-slots (class-of node))))
    (flet ((init-slot (slot value)
             (let ((name (c2mop:slot-definition-name slot)))
               (if (port-type slot)
                   (let ((port (if (slot-boundp node name)
                                   (slot-value node name)
                                   (make-instance (port-type slot) :node node))))
                     (unless (eql (type-of port) (port-type slot))
                       (change-class port (port-type slot)))
                     (setf (connections port) value)
                     (setf (slot-value node name) port))
                   (setf (slot-value node name) value)))))
      ;; Process initargs
      (loop with initialized = ()
            for (key value) on initargs by #'cddr
            for slots = (find-slots-by-initarg key slots)
            do (dolist (slot slots)
                 ;; See §7.1.4 Rules for Initialization Arguments
                 (unless (find slot initialized)
                   (init-slot slot value)
                   (push slot initialized))))
      ;; Process initforms
      (loop for name in initform-slots
            for slot = (find-slot-by-name name slots)
            do (unless (slot-boundp node name)
                 (init-slot slot (eval (c2mop:slot-definition-initform slot)))))
      node)))

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

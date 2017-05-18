#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flow)

(defvar *resolve-port* T)

(defclass unit ()
  ())

(defclass connection (unit)
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right)))

(defmethod connection= (a b)
  (or (and (eql (left a) (left b))
           (eql (right a) (right b)))
      (and (eql (left a) (right b))
           (eql (right a) (left b)))))

(defmethod sever ((connection connection))
  (remove-connection connection (left connection))
  (remove-connection connection (right connection))
  connection)

(defclass directed-connection (connection)
  ())

(defmethod connection= ((a directed-connection) (b directed-connection))
  (or (and (eql (left a) (left b))
           (eql (right a) (right b)))))

(defclass port (unit)
  ((connections :initarg :connections :initform () :accessor connections)
   (node :initarg :node :initform NIL :accessor node)))

(defmethod connect ((left port) (right port) &optional (connection-type 'connection) &rest initargs)
  (let ((connection (apply #'make-instance connection-type :left left :right right initargs)))
    (check-connection-accepted connection left)
    (check-connection-accepted connection right)
    (push connection (connections left))
    (push connection (connections right))
    connection))

(defmethod disconnect ((left port) (right port))
  (let ((connection (make-instance 'directed-connection :left left :right right)))
    (remove-connection connection left :test #'connection=)
    (remove-connection connection right :test #'connection=)
    NIL))

(defmethod remove-connection (connection (port port) &key (test #'eql))
  (setf (connections port) (remove connection (connections port) :test test)))

(defmethod check-connection-accepted (new-connection (port port))
  (loop for connection in (connections port)
        do (when (connection= connection new-connection)
             (error "An equivalent connection already exists."))))

(defmethod sever ((port port))
  (mapc #'sever (connections port)))

(defclass n-port (port)
  ())

(defclass 1-port (port)
  ())

(defmethod check-connection-accepted (connection (port 1-port))
  (call-next-method)
  (when (connections port)
    (error "A connection already exists on this port.")))

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
    (unless (port-type slot)
      (error "The slot ~a in ~a is not a port." name (class-name node)))
    (let ((*resolve-port* NIL))
      (slot-value node name))))

(defmethod c2mop:slot-value-using-class ((node node-class) object (slot effective-port-definition))
  (let ((port (call-next-method)))
    (if (and (port-type slot) *resolve-port*)
        (connections port)
        port)))

(defmethod (setf c2mop:slot-value-using-class) (value (node node-class) object (slot effective-port-definition))
  (if (and (port-type slot) *resolve-port*)
      (setf (connections (c2mop:slot-value-using-class node object slot)) value)
      (call-next-method)))

(defmethod c2mop:slot-boundp-using-class ((node node-class) object (slot effective-port-definition))
  (if (and (port-type slot) *resolve-port*)
      (slot-boundp (c2mop:slot-value-using-class node object slot) 'connections)
      (call-next-method)))

(defmethod c2mop:slot-makunbound-using-class ((node node-class) object (slot effective-port-definition))
  (if (and (port-type slot) *resolve-port*)
      (slot-makunbound (c2mop:slot-value-using-class node object slot) 'connections)
      (call-next-method)))

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
            for initfunction = (c2mop:slot-definition-initfunction slot)
            do (unless (slot-boundp node name)
                 (when initfunction (init-slot slot (funcall initfunction)))))
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

(defmethod sever ((node node))
  (mapc #'sever (ports node)))

(defmethod connections ((node node))
  (reduce #'append (ports node) :key #'connections))

(defmethod remove-connection (connection (node node) &key (test #'eql))
  (dolist (port (ports node))
    (remove-connection connection port :test test))
  connection)

(defmethod disconnect ((node node) (port port))
  (dolist (other-port (ports node))
    (disconnect other-port port)))

(defmethod disconnect ((port port) (node node))
  (dolist (other-port (ports node))
    (disconnect port other-port)))

(defmethod disconnect ((a node) (b node))
  (dolist (a-port (ports a))
    (dolist (b-port (ports b))
      (disconnect a-port b-port))))

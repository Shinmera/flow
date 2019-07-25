#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flow)

(defvar *resolve-port* T)

(defmacro define-port-value-slot (port-class slot &optional accessor)
  `(progn (defmethod port-value ((,port-class ,port-class))
            ,(if accessor
                 `(,accessor ,port-class)
                 `(slot-value ,port-class ,slot)))
          
          (defmethod (setf port-value) (value (,port-class ,port-class))
            (setf ,(if accessor
                       `(,accessor ,port-class)
                       `(slot-value ,port-class ,slot))
                  value))

          (defmethod port-value-boundp ((,port-class ,port-class))
            (slot-boundp ,port-class ',slot))

          (defmethod port-value-makunbound ((,port-class ,port-class))
            (slot-makunbound ,port-class ',slot))))

(define-port-value-slot port connections connections)

(defclass port-definition ()
  ((port-type :initarg :port-type :accessor port-type)
   (port-initargs :initform () :accessor port-initargs))
  (:default-initargs
   :port-type NIL))

(defmethod port-type ((slot c2mop:slot-definition))
  NIL)

(defclass direct-port-definition (port-definition c2mop:standard-direct-slot-definition)
  ())

(defmethod initialize-instance :after ((definition port-definition) &rest initargs &key &allow-other-keys)
  (let ((initargs (copy-list initargs)))
    (dolist (attr '(:initargs :initform :initfunction :allocation
                    :accessor :readers :writers :port-type
                    :type :documentation :class :name #+sbcl SB-PCL::SOURCE))
      (remf initargs attr))
    (setf (port-initargs definition) initargs)))

(defclass effective-port-definition (port-definition c2mop:standard-effective-slot-definition)
  ())

(defclass static-node-class (standard-class)
  ())

(defmethod c2mop:validate-superclass ((class static-node-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass static-node-class))
  T)

(defmethod c2mop:validate-superclass ((class static-node-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class static-node-class) (superclass static-node-class))
  T)

(defmethod c2mop:compute-effective-slot-definition ((class static-node-class) name direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (loop for direct-slot in direct-slots
          do (when (and (typep direct-slot 'port-definition)
                        (eql (c2mop:slot-definition-name direct-slot)
                             (c2mop:slot-definition-name effective-slot)))
               (setf (port-type effective-slot) (port-type direct-slot))
               (setf (port-initargs effective-slot) (port-initargs direct-slot))
               (loop-finish)))
    effective-slot))

(defmethod c2mop:direct-slot-definition-class ((class static-node-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-port-definition))

(defmethod c2mop:effective-slot-definition-class ((class static-node-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'effective-port-definition))

(defmethod c2mop:slot-value-using-class ((node static-node-class) object (slot effective-port-definition))
  (let ((port (call-next-method)))
    (if (and (port-type slot) *resolve-port*)
        (port-value port)
        port)))

(defmethod (setf c2mop:slot-value-using-class) (value (node static-node-class) object (slot effective-port-definition))
  (if (and (port-type slot) *resolve-port*)
      (let ((*resolve-port* NIL))
        (setf (port-value (c2mop:slot-value-using-class node object slot)) value))
      (call-next-method)))

(defmethod c2mop:slot-boundp-using-class ((node static-node-class) object (slot effective-port-definition))
  (if (and (port-type slot) *resolve-port*)
      (and (call-next-method)
           (let ((*resolve-port* NIL))
             (port-value-boundp (c2mop:slot-value-using-class node object slot))))
      (call-next-method)))

(defmethod c2mop:slot-makunbound-using-class ((node static-node-class) object (slot effective-port-definition))
  (if (and (port-type slot) *resolve-port*)
      (port-value-makunbound
       (let ((*resolve-port* NIL))
         (c2mop:slot-value-using-class node object slot)))
      (call-next-method)))

(defun port-slot-value (node name)
  (let ((*resolve-port* NIL))
    (slot-value node name)))

(defun (setf port-slot-value) (value node name)
  (let ((*resolve-port* NIL))
    (setf (slot-value node name) value)))

(defun port-slot-boundp (node name)
  (let ((*resolve-port* NIL))
    (slot-boundp node name)))

(defclass static-node (node)
  ()
  (:metaclass static-node-class))

(defmethod shared-initialize ((node static-node) initform-slots &rest initargs)
  (let ((slots (c2mop:class-slots (class-of node))))
    (flet ((init-slot (slot value)
             (let ((name (c2mop:slot-definition-name slot)))
               (if (port-type slot)
                   (let ((port (if (port-slot-boundp node name)
                                   (slot-value node name)
                                   (apply #'make-instance (port-type slot)
                                          :node node :name name (port-initargs slot)))))
                     (apply #'change-class port (port-type slot) (port-initargs slot))
                     (setf (connections port) value)
                     (setf (port-slot-value node name) port))
                   (setf (slot-value node name) value)))))
      ;; FIXME: handle conversion of slots between non-port-type and port-type
      ;; Process initargs
      (loop with initialized = ()
            for (key value) on initargs by #'cddr
            for init-slots = (find-slots-by-initarg key slots)
            do (dolist (slot init-slots)
                 ;; See §7.1.4 Rules for Initialization Arguments
                 (unless (find slot initialized)
                   (init-slot slot value)
                   (push slot initialized))))
      ;; Process initforms
      (when (eql initform-slots T)
        (setf initform-slots (mapcar #'c2mop:slot-definition-name slots)))
      (loop for name in initform-slots
            for slot = (find-slot-by-name name slots)
            for initfunction = (c2mop:slot-definition-initfunction slot)
            do (unless (slot-boundp node name)
                 (cond (initfunction
                        (init-slot slot (funcall initfunction)))
                       ((port-type slot)
                        (init-slot slot NIL)))))
      node)))

(defmacro define-node (name direct-superclasses direct-slots &rest options)
  (unless (find :metaclass options :key #'first)
    (push `(:metaclass static-node-class) options))
  `(defclass ,name (,@direct-superclasses static-node)
     ,direct-slots
     ,@options))

(defmethod ports ((node static-node))
  (loop for slot in (c2mop:class-slots (class-of node))
        when (port-type slot)
        collect (port-slot-value node (c2mop:slot-definition-name slot))))

(defmethod port ((node static-node) (name symbol))
  (let ((slot (find name (c2mop:class-slots (class-of node))
                    :key #'c2mop:slot-definition-name)))
    (unless (and (typep slot 'port-definition) (port-type slot))
      (error 'designator-not-a-port :port-name name :node node))
    (port-slot-value node name)))

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

;; SIGH. Why oh why do we have to replicate this shit just to customise the effective slot definition
;; class conditionally.
(defun compute-effective-slot-definition-initargs (direct-slotds)
  (let ((args (list :name (c2mop:slot-definition-name (first direct-slotds)) :type T))
        (_ '#:no-value))
    (dolist (slotd direct-slotds args)
      (when slotd
        (when (and (eq _ (getf args :initfunction _)) (c2mop:slot-definition-initfunction slotd))
          (setf (getf args :initfunction) (c2mop:slot-definition-initfunction slotd))
          (setf (getf args :initform) (c2mop:slot-definition-initform slotd)))
        (when (and (eq _ (getf args :documentation _)) (documentation slotd T))
          (setf (getf args :documentation) (documentation slotd T)))
        (when (and (eq _ (getf args :allocation _)) (c2mop:slot-definition-allocation slotd))
          (setf (getf args :allocation) (c2mop:slot-definition-allocation slotd)))
        (setf (getf args :initargs) (union (getf args :initargs) (c2mop:slot-definition-initargs slotd)))
        (let ((slotd-type (c2mop:slot-definition-type slotd)))
          (setf (getf args :type) (cond ((eq (getf args :type) T) slotd-type)
                                        (T `(and ,slotd-type ,(getf args :type))))))))))

(defmethod c2mop:compute-effective-slot-definition ((class static-node-class) name direct-slots)
  (declare (ignore name))
  (let* ((initargs (compute-effective-slot-definition-initargs direct-slots))
         (slot (loop for direct in direct-slots
                     do (when (and (typep direct 'port-definition) (port-type direct)) (return direct)))))
    (cond (slot
           (setf initargs (list* :port-type (port-type slot) (append (port-initargs slot) initargs)))
           (apply #'make-instance (apply #'c2mop:effective-slot-definition-class class initargs) initargs))
          (T
           (call-next-method)))))

(defmethod c2mop:direct-slot-definition-class ((class static-node-class) &rest initargs &key port-type)
  (declare (ignore initargs))
  (if port-type
      (find-class 'direct-port-definition)
      (call-next-method)))

(defmethod c2mop:effective-slot-definition-class ((class static-node-class) &rest initargs &key port-type port-initargs)
  (declare (ignore initargs port-initargs))
  (if port-type
      (find-class 'effective-port-definition)
      (call-next-method)))

(defmethod c2mop:slot-value-using-class ((node static-node-class) object (slot effective-port-definition))
  (let ((port (call-next-method)))
    (if (and (port-type slot) *resolve-port*)
        (port-value port)
        port)))

(defmethod (setf c2mop:slot-value-using-class) (value (node static-node-class) object (slot effective-port-definition))
  (if (and (port-type slot) *resolve-port*)
      (setf (port-value (port-slot-value object slot)) value)
      (call-next-method)))

(defmethod c2mop:slot-boundp-using-class ((node static-node-class) object (slot effective-port-definition))
  (if (and (port-type slot) *resolve-port*)
      (and (call-next-method)
           (port-value-boundp (port-slot-value object slot)))
      (call-next-method)))

(defmethod c2mop:slot-makunbound-using-class ((node static-node-class) object (slot effective-port-definition))
  (if (and (port-type slot) *resolve-port*)
      (port-value-makunbound (port-slot-value object slot))
      (call-next-method)))

(defmethod change-class :around (instance (node static-node-class) &rest initargs)
  (declare (ignore initargs))
  ;; The implementation might touch the slots with s-v-u-c during class change, and
  ;; clobber the fields that way. To prevent this we revert to using standard method
  ;; access during this time.
  (let ((*resolve-port* NIL))
    (call-next-method)))

(defun port-slot-value (node slot)
  (c2mop:standard-instance-access node (c2mop:slot-definition-location slot)))

(defun (setf port-slot-value) (value node slot)
  (setf (c2mop:standard-instance-access node (c2mop:slot-definition-location slot))
        value))

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
                     (setf (port-slot-value node slot) port))
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
        collect (port-slot-value node slot)))

(defmethod port ((node static-node) (name symbol))
  (let ((slot (find name (c2mop:class-slots (class-of node))
                    :key #'c2mop:slot-definition-name)))
    (unless (and (typep slot 'port-definition) (port-type slot))
      (error 'designator-not-a-port :port-name name :node node))
    (port-slot-value node slot)))

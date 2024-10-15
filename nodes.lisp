(in-package #:org.shirakumo.flow)

(defclass unit ()
  ((attributes :initform (make-hash-table :test 'eql) :accessor attributes)))

(defmethod attribute ((unit unit) name &optional default)
  (gethash name (attributes unit) default))

(defmethod (setf attribute) (value (unit unit) name)
  (setf (gethash name (attributes unit)) value))

(defmethod remove-attribute ((unit unit) name)
  (remhash name (attributes unit)))

(defmacro with-attributes (attributes unit &body body)
  (let ((unitg (gensym "UNIT")))
    `(let ((,unitg ,unit))
       (symbol-macrolet ,(loop for attribute in attributes
                               collect `(,attribute (attribute ,unitg ',attribute)))
         ,@body))))

(defclass connection (unit)
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right)))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type T)
    (format stream "~a <-> ~a"
            (left connection)
            (right connection))))

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

(defmethod print-object ((connection directed-connection) stream)
  (print-unreadable-object (connection stream :type T)
    (format stream "~a --> ~a"
            (left connection)
            (right connection))))

(defmethod connection= ((a directed-connection) (b directed-connection))
  (or (and (eql (left a) (left b))
           (eql (right a) (right b)))))

(defclass port (unit)
  ((connections :initarg :connections :initform () :accessor connections)
   (node :initarg :node :initform NIL :accessor node)
   (name :initarg :name :initform NIL :accessor name)))

(defmethod print-object ((port port) stream)
  (print-unreadable-object (port stream :type T)
    (format stream "~a/~a" (node port) (name port))))

(defmethod describe-object :after ((port port) stream)
  (format stream "~&~%Connections:~%")
  (dolist (connection (connections port))
    (let ((other (if (eq port (left connection)) (right connection) (left connection))))
      (cond ((not (typep connection 'directed-connection))
             (format stream " --- "))
            ((eq port (left connection))
             (format stream " --> "))
            (T
             (format stream " <-- ")))
      (format stream "~a ~a~%" (name other) (node other)))))

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

(defgeneric check-connection-accepted (connection port)
  (:method-combination progn))

(defmethod check-connection-accepted progn (new-connection (port port))
  (loop for connection in (connections port)
        do (when (connection= connection new-connection)
             (error 'connection-already-exists
                    :new-connection new-connection
                    :old-connection connection))))

(defmethod sever ((port port))
  (mapc #'sever (connections port)))

(defclass n-port (port)
  ())

(defclass 1-port (port)
  ())

(defmethod check-connection-accepted progn (connection (port 1-port))
  (when (connections port)
    (error 'connection-already-exists
           :new-connection connection
           :old-connection (first (connections port)))))

(defclass in-port (port)
  ())

(defmethod check-connection-accepted progn ((connection directed-connection) (port in-port))
  (unless (eql port (right connection))
    (error 'illegal-connection :connection connection :message "Only incoming connections are allowed.")))

(defclass out-port (port)
  ())

(defmethod check-connection-accepted progn ((connection directed-connection) (port out-port))
  (unless (eql port (left connection))
    (error 'illegal-connection :connection connection :message "Only outgoing connections are allowed.")))

(defclass node (unit)
  ())

(defmethod describe-object :after ((node node) stream)
  (format stream "~&~%")
  (flet ((filter-ports (type)
           (loop for port in (ports node)
                 when (typep port type)
                 collect (if (slot-boundp node (name port))
                             (cons (name port) (slot-value node (name port)))
                             (name port)))))
    (org.shirakumo.text-draw:node
     (filter-ports 'in-port) (filter-ports 'out-port) :stream stream)))

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

(defclass dynamic-node (node)
  ((ports :initarg :ports :initform () :accessor ports)))

(defmethod port ((node dynamic-node) (name symbol))
  (or (find name (ports node) :key #'name)
      (error 'designator-not-a-port :port-name name :node node)))

(defun other-node (node connection)
  (let ((right (flow:node (flow:right connection))))
    (if (eq right node)
        (flow:node (flow:left connection))
        right)))

(defun target-node (node connection)
  (let ((left (flow:node (flow:left connection))))
    (if (eq left node)
        (flow:node (flow:right connection))
        (if (typep connection 'directed-connection)
            NIL
            left))))

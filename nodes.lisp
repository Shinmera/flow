#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
             (error "An equivalent connection already exists."))))

(defmethod sever ((port port))
  (mapc #'sever (connections port)))

(defclass n-port (port)
  ())

(defclass 1-port (port)
  ())

(defmethod check-connection-accepted progn (connection (port 1-port))
  (when (connections port)
    (error "A connection already exists on this port.")))

(defclass in-port (port)
  ())

(defmethod check-connection-accepted progn ((connection directed-connection) (port in-port))
  (unless (eql port (right connection))
    (error "This port only allows incoming connections.")))

(defclass out-port (port)
  ())

(defmethod check-connection-accepted progn ((connection directed-connection) (port out-port))
  (unless (eql port (left connection))
    (error "This port only allows outgoing connections.")))

(defclass node (unit)
  ())

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
  (find name (ports node) :key #'name))

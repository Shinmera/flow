#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flow)

(defun visit (node function)
  (let ((visited (make-hash-table :test 'eq)))
    (labels ((%visit (node)
               (unless (gethash node visited)
                 (setf (gethash node visited) node)
                 (funcall function node)
                 (dolist (connection (connections node))
                   (cond ((eql node (node (left connection)))
                          (%visit (node (right connection))))
                         ((eql node (node (right connection)))
                          (%visit (node (left connection)))))))))
      (%visit node))))

(defun extract-graph (node)
  (let ((vertices ())
        (edges ()))
    (flet ((connect (left right)
             (pushnew (list left right) edges :test #'equal)))
      (visit node (lambda (node)
                    (push node vertices)
                    (dolist (connection (connections node))
                      (etypecase connection
                        (directed-connection
                         (connect (node (left connection)) (node (right connection))))
                        (connection
                         (connect (node (left connection)) (node (right connection)))
                         (connect (node (right connection)) (node (left connection))))))))
      (values vertices edges))))

(defun topological-sort (node)
  (let ((sorted ())
        (visited (make-hash-table :test 'eq)))
    (labels ((%visit (node)
               (case (gethash node visited)
                 (:temporary
                  (error "The graph contains cycles."))
                 (:permanently)
                 ((NIL)
                  (setf (gethash node visited) :temporary)
                  (dolist (connection (connections node))
                    (etypecase connection
                      (directed-connection
                       (when (eql node (left connection))
                         (%visit (right connection))))
                      (connection
                       (cond ((eql node (left connection))
                              (%visit (right connection)))
                             ((eql node (right connection))
                              (%visit (left connection)))))))
                  (setf (gethash node visited) :permanently)
                  (push node sorted)))))
      (%visit node))
    sorted))

(defun color-graph (vertices edges &optional (attribute :color))
  (let ((available (make-array (length vertices) :initial-element T)))
    (flet ((mark-adjacent (vertex how)
             (loop for (from . to) in edges
                   do (cond ((eql vertex from)
                             (let ((color (attribute to attribute)))
                               (when color (setf (aref available color) how))))
                            ((eql vertex to)
                             (let ((color (attribute from attribute)))
                               (when color (setf (aref available color) how))))))))
      (dolist (vertex vertices vertices)
        (mark-adjacent vertex NIL)
        (setf (attribute vertex attribute)
              (dotimes (i (length available))
                (when (aref available i) (return i))))
        (mark-adjacent vertex T)))))

(defun color-nodes (root-node &optional (attribute :color))
  (multiple-value-bind (vertices edges)
      (extract-graph root-node)
    (color-graph vertices edges attribute)))

(defun color-ports (node &optional (attribute :color))
  (let ((vertices ())
        (edges ()))
    ;; We define a custom graph building here that makes a
    ;; graph composed solely out of the ports of the nodes.
    ;; We specially treat ports marked as "in-ports" as
    ;; they should not receive a colour of their own.
    ;; Instead their connections are rerouted to every non-
    ;; in-port in the same node.
    (flet ((connect (left right)
             (pushnew (list left right) edges :test #'equal)))
      (visit node (lambda (node)
                    (dolist (port (ports node))
                      (etypecase port
                        (in-port
                         ;; in-ports are pass-through to every other port in the node
                         (dolist (connection (connections port))
                           (dolist (other-port (ports node))
                             (unless (typep other-port 'in-port)
                               (etypecase connection
                                 (directed-connection
                                  (connect (left connection) other-port))
                                 (connection
                                  (cond ((eql port (left connection))
                                         (connect other-port (right connection))
                                         (connect (right connection) other-port))
                                        ((eql port (right connection))
                                         (connect (left connection) other-port)
                                         (connect other-port (left connection))))))))))
                        (port
                         (dolist (connection (connections port))
                           (etypecase connection
                             (directed-connection
                              (connect (left connection) (right connection)))
                             (connection
                              (connect (left connection) (right connection))
                              (connect (right connection) (left connection)))))))))))
    (color-graph vertices edges attribute)))

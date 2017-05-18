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
                   (cond ((eql node (left connection))
                          (%visit (right connection)))
                         ((eql node (right connection))
                          (%visit (left connection))))))))
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
                         (connect (left connection) (right connection)))
                        (connection
                         (connect (left connection) (right connection))
                         (connect (right connection) (left connection)))))))
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

(defun color-nodes (root-node))

(defun color-outputs (root-node))

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

(defun topological-sort (nodes)
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
                       (when (eql node (node (left connection)))
                         (%visit (node (right connection)))))
                      (connection
                       (cond ((eql node (node (left connection)))
                              (%visit (node (right connection))))
                             ((eql node (node (right connection)))
                              (%visit (node (left connection))))))))
                  (setf (gethash node visited) :permanently)
                  (push node sorted)))))
      (dolist (node nodes)
        (%visit node)))
    sorted))

(defun color-nodes (node &key (attribute :color) (clear T))
  (multiple-value-bind (vertices edges)
      (extract-graph node)
    (let ((colors (make-array (length vertices) :initial-element :available)))
      (flet ((mark-adjacent (vertex how)
               (loop for (from to) in edges
                     do (cond ((eql vertex from)
                               (let ((color (attribute to attribute)))
                                 (when color (setf (aref colors color) how))))
                              ((eql vertex to)
                               (let ((color (attribute from attribute)))
                                 (when color (setf (aref colors color) how))))))))
        (when clear
          (dolist (vertex vertices)
            (remove-attribute vertex attribute)))
        (dolist (vertex vertices vertices)
          (mark-adjacent vertex :unavailable)
          (setf (attribute vertex attribute) (position :available colors))
          (mark-adjacent vertex :available))))))

(defun allocate-ports (nodes &key (attribute :color) (clear T) (in-place-attribute :in-place) test (sort #'topological-sort))
  (let ((test (or test (constantly T)))
        (nodes (funcall (or sort #'identity) nodes))
        (length 0))
    (flet ((color (port) (attribute port attribute))
           ((setf color) (value port) (setf (attribute port attribute) value))
           (applicable-p (port) (funcall test port)))
      ;; Clear and count number of ports.
      (dolist (node nodes nodes)
        (dolist (port (ports node))
          (unless (typep port 'in-port)
            (incf length))
          (when clear (setf (color port) NIL))))
      ;; Perform the actual colouring.
      (let ((colors (make-array length :initial-element :available)))
        (dolist (node (reverse nodes) nodes)
          ;; If we have a port that is in-place we
          ;; immediately release the colours to allow them
          ;; to be re-used in predecessor ports.
          (dolist (port (ports node))
            (when (and (applicable-p port) (color port) (attribute port in-place-attribute))
              (setf (aref colors (color port)) :available)))
          ;; Distribute colours across predecessor ports.
          (dolist (port (ports node))
            (when (typep port 'in-port)
              (dolist (connection (connections port))
                (let ((other (if (eql port (left connection))
                                 (right connection)
                                 (left connection))))
                  (when (and (applicable-p port) (not (color other)))
                    (let ((color (position :available colors)))
                      (setf (color other) color)
                      (setf (aref colors color) :unavailable)))))))
          ;; Distribute colours across internal ports.
          ;; This only happens if a node has unconnected ports.
          (dolist (port (ports node))
            (unless (typep port 'in-port)
              (when (and (applicable-p port) (not (color port)))
                (let ((color (position :available colors)))
                  (setf (color port) color)
                  (setf (aref colors color) :unavailable)))))
          ;; Mark own as available again.
          (dolist (port (ports node))
            (when (and (applicable-p port) (color port))
              (setf (aref colors (color port)) :available))))))))

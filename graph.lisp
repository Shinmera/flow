#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flow)

(defgeneric visit (root-node function))
(defgeneric extract-graph (root-node))
(defun topological-sort (root-node))
(defun color-nodes (root-node))
(defun color-outputs (root-node))

#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flow)

(defun find-slots-by-initarg (key slots)
  (loop for slot in slots
        when (find key (c2mop:slot-definition-initargs slot))
        collect slot))

(defun find-slot-by-name (name slots)
  (find name slots :key #'c2mop:slot-definition-name))

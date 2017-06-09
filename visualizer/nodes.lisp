#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:flow-visualizer)
(in-readtable :qtools)

(defun filter-by-type (type list)
  (remove-if-not (lambda (el) (typep el type)) list))

(defmethod location ((node node))
  (q+:make-qpointf (attribute node 'x)
                   (attribute node 'y)))

(defmethod location ((port port))
  (location (node port)))

(defmethod paint ((node node) painter)
  (with-finalizing ((rect (q+:make-qrectf
                           (attribute node 'x)
                           (attribute node 'y)
                           (attribute node 'w)
                           (attribute node 'h)))
                    (pen (q+:make-qpen (q+:make-qcolor 0 0 0)))
                    (back (q+:make-qbrush (q+:make-qcolor 255 255 255))))
    (setf (q+:pen painter) pen)
    (setf (q+:brush painter) back)
    (q+:draw-rect painter rect)
    (q+:draw-text painter rect (q+:qt.align-center)
                  (princ-to-string (or (attribute node 'text)
                                       node))
                  rect)
    (dolist (port (ports node))
      (paint port painter))))

(defmethod paint ((port port) painter)
  (let ((node (node port))
        (point (location port)))
    (setf (q+:clipping painter) T)
    (setf (q+:clip-rect painter) (q+:make-qrectf
                                  (+ (attribute node 'x) 1)
                                  (+ (attribute node 'y) 1)
                                  (- (attribute node 'w) 1)
                                  (- (attribute node 'h) 1)))
    (q+:draw-ellipse painter point 5. 5.)
    (setf (q+:clipping painter) NIL)
    (q+:draw-text painter point (princ-to-string (or (attribute port 'text)
                                                     (name port))))
    (dolist (connection (connections port))
      (when (eql port (left connection))
        (paint connection painter)))))

(defmethod paint ((connection connection) painter)
  (q+:draw-line painter (location (left connection)) (location (right connection))))

(defclass in-port (n-port)
  ())

(defmethod location ((port in-port))
  (let* ((point (location (node port)))
         (in-ports (filter-by-type 'in-port (ports (node port))))
         (dist (/ (attribute (node port) 'w) (length in-ports))))
    (setf (q+:x point)
          (+ (q+:x point)
             (* dist (position port in-ports))
             (/ dist 2)))
    point))

(defclass out-port (1-port)
  ())

(defmethod location ((port out-port))
  (let* ((point (location (node port)))
         (out-ports (filter-by-type 'out-port (ports (node port))))
         (dist (/ (attribute (node port) 'w) (length out-ports))))
    (setf (q+:x point)
          (+ (q+:x point)
             (* dist (position port out-ports))
             (/ dist 2)))
    (setf (q+:y point)
          (+ (q+:y point)
             (attribute (node port) 'h)))
    point))

(define-node start ()
  ((out :port-type out-port)))

(define-node end ()
  ((in :port-type in-port)))

(define-node process ()
  ((in :port-type in-port)
   (out :port-type out-port)))

(define-node conditional ()
  ((in :port-type in-port)
   (true :port-type out-port)
   (false :port-type out-port)))

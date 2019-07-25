#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flow)

(define-condition flow-condition (condition)
  ())

(define-condition connection-already-exists (flow-condition error)
  ((new-connection :initarg :new-connection :reader new-connection)
   (old-connection :initarg :old-connection :reader old-connection))
  (:report (lambda (c s) (format s "The connection~%  ~a~%already exists and is considered equal to~%  ~a"
                                 (old-connection c) (new-connection c)))))

(define-condition illegal-connection (flow-condition error)
  ((connection :initarg :connection :reader connection)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "The connection~%  ~a~%is not allowed~:[.~;~:*: ~a~]"
                                 (connection c) (message c)))))

(define-condition designator-not-a-port (flow-condition error)
  ((node :initarg :node :reader node)
   (port-name :initarg :port-name :reader port-name))
  (:report (lambda (c s) (format s "The name ~s does not designate a port on ~a."
                                 (port-name c) (node c)))))

(define-condition graph-structure-error (flow-condition error)
  ())

(define-condition graph-contains-cycles (graph-structure-error)
  ((node :initarg :node :reader node))
  (:report "The graph contains cycles."))

(define-condition graph-is-bipartite (graph-structure-error)
  ((node-a :initarg :node-a :reader node-a)
   (node-b :initarg :node-b :reader node-b))
  (:report (lambda (c s) (format s "The nodes~%  ~a~%and~%  ~a~%are in bipartite graphs."
                                 (node-a c) (node-b c)))))

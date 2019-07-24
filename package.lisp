#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:flow
  (:nicknames #:org.shirakumo.flow)
  (:use #:cl)
  ;; graph.lisp
  (:export
   #:visit
   #:extract-graph
   #:topological-sort
   #:color-nodes
   #:allocate-ports)
  ;; nodes.lisp
  (:export
   #:unit
   #:attributes
   #:attribute
   #:remove-attribute
   #:with-attributes
   #:connection
   #:left
   #:right
   #:connection=
   #:sever
   #:directed-connection
   #:port
   #:connections
   #:node
   #:name
   #:connect
   #:disconnect
   #:remove-connection
   #:check-connection-accepted
   #:n-port
   #:1-port
   #:in-port
   #:out-port
   #:node
   #:ports
   #:port
   #:dynamic-node
   #:other-node
   #:target-node)
  ;; static-node.lisp
  (:export
   #:*resolve-port*
   #:port-value
   #:port-value-boundp
   #:port-value-makunbound
   #:define-port-value-slot
   #:port-definition
   #:port-type
   #:static-node-class
   #:static-node
   #:define-node)
  ;; toolkit.lisp
  (:export))

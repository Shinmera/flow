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
   #:color-outputs)
  ;; nodes.lisp
  (:export
   #:*resolve-port*
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
   #:slot
   #:connect
   #:disconnect
   #:remove-connection
   #:check-connection-accepted
   #:n-port
   #:1-port
   #:in-port
   #:out-port
   #:port-definition
   #:port-type
   #:node-class
   #:ports
   #:port
   #:node
   #:define-node)
  ;; toolkit.lisp
  (:export))

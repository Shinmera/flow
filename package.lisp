(in-package #:cl-user)
(defpackage #:flow
  (:nicknames #:org.shirakumo.flow)
  (:use #:cl)
  ;; conditions.lisp
  (:export
   #:flow-condition
   #:connection-already-exists
   #:new-connection
   #:old-connection
   #:illegal-connection
   #:connection
   #:message
   #:designator-not-a-port
   #:node
   #:slot-name
   #:graph-structure-error
   #:graph-contains-cycles
   #:node
   #:graph-is-bipartite
   #:node-a
   #:node-b)
  ;; graph.lisp
  (:export
   #:visit
   #:extract-graph
   #:topological-sort
   #:color-nodes
   #:allocate-ports
   #:a*)
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

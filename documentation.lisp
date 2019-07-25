#|
 This file is a part of flow
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flow)

;; conditions.lisp
(docs:define-docs
  (type flow-condition
    "Base type for all conditions from the Flow library.")
  
  (type connection-already-exists
    "Error signalled if an equivalent connection is added.

See NEW-CONNECTION
See OLD-CONNECTION
See FLOW-CONDITION")
  
  (function new-connection
    "Returns the new connection that was attempted to be added.

See CONNECTION-ALREADY-EXISTS")
  
  (function old-connection
    "Returns the old connection that already exists on the ports.

See CONNECTION-ALREADY-EXISTS")
  
  (type illegal-connection
    "Error signalled if the connection is not permitted by the ports.

See CONNECTION
See MESSAGE
See FLOW-CONDITION")
  
  (function connection
    "Returns the connection that could not be added.

See ILLEGAL-CONNECTION")
  
  (function message
    "Returns a reason for the failure.

See ILLEGAL-CONNECTION")
  
  (type designator-not-a-port
    "Error signalled when a port is accessed that does not exist.

See NODE
See PORT-NAME")
  
  (function node
    "Returns the node the failure is associated with.

See DESIGNATOR-NOT-A-PORT")
  
  (function port-name
    "Returns the name of the port that was attempted to be accessed.

See DESIGNATOR-NOT-A-PORT")
  
  (type graph-structure-error
    "Base type for conditions related to structural problems in graphs.

These conditions are used by the various generic
algorithms offered in Flow to signal that a
precondition of an operation is not fulfilled in
some way.

See FLOW-CONDITION")
  
  (type graph-contains-cycles
    "Error signalled if the graph is cyclic.

This error is signalled on algorithms that expect
an acyclic graph.

See NODE
See GRAPH-STRUCTURE-ERROR")
  
  (type graph-is-bipartite
    "Error signalled if the graph is bipartite.

This error is signalled on algorithms that expect
a singular, connected graph.

See NODE-A
See NODE-B
See GRAPH-STRUCTURE-ERROR")

  (function node-a
    "Returns the first node associated with the failure.

See GRAPH-IS-BIPARTITE")

  (function node-b
    "Returns the second node associated with the failure.

See GRAPH-IS-BIPARTITE"))

;; graph.lisp
(docs:define-docs
  (function visit
    "Visit each node in the graph, starting from the given node.

The visiting proceeds by calling the function on a
node, then recursing through each connection of the
node. The recursion does not respect directed
connections. It is guaranteed that each node is
only visited once, regardless of cycles.")
  
  (function extract-graph
    "Extract the graph starting from the given node.

This returns two lists, the first being the list of
vertices (nodes), and the second being the list of
edges, with each edge being a list of left and right
vertex that are connected. The edges are intended to
be directed. Undirected edges are represented by
two edges, one from left to right and one from right
to left.

The order of the vertices and edges in the returned
lists is unspecified.

See VISIT")
  
  (function topological-sort
    "Produces a topological sorting of the given nodes.

This uses Tarjan's algorithm to compute the
topological sorting. Note that if the given list
of nodes does not include all reachable nodes, the
result may be erroneous.

Signals an error of type GRAPH-CONTAINS-CYCLES if the
graph contains cycles.

See GRAPH-CONTAINS-CYCLES")
  
  (function color-nodes
    "Perform a graph colouring.

Each node in the graph from the given starting node
out is assigned a \"colour\" to the specified
attribute. This colour is in practise an integer in
the range [0,n] where n is the number of nodes in
the graph. The colours are distributed in such a way
that no neighbouring nodes have the same colour.

The employed algorithm is greedy and cannot guarantee
an optimal colouring. Optimal colouring is an NP-
complete problem, and the results produced by a
greedy algorithm are usually shown to be good enough.

The full list of coloured nodes is returned.")
  
  (function allocate-ports
    "Perform a colour \"allocation\" on the ports of the graph.

Each port reachable in the graph from the given
starting nodes out that is not of type in-port is
assigned a \"colour\" to the specified attribute.
If clear is non-NIL, the colour attribute is first
cleared off of each port, ensuring a clean colouring.

The colouring rules are as follows:
A port may not have the same colour as any of the
other ports on the same node. Unless the node's
in-place-attribute is non-NIL, the colour must
also be distinct from the colour of any of the
node's predecessor ports. A predecessor port being
any port that is connected to an in-port of the
node.

In effect this produces a colouring that is useful
to calculate the allocation of buffers and other
resources necessary to perform a calculation for
a node. These rules ensure that the calculation
can be performed without accidentally overwriting
buffer data necessary at a later point in the
execution of the graph, while at the same time
also minimising the number of necessary buffers.

The given graph may not contain any cycles.

Before the nodes are processed, they are sorted by
SORT, defaulting to a TOPOLOGICAL-SORT. The sorted
nodes must be in such an order that the nodes
appear in a topological order.

If TEST is given, only ports for which the TEST
function returns non-NIL are considered for the
colouring. This allows you to distribute multiple
colour \"kinds\" across a single graph by running
the colouring once for each kind of colour and
excluding the ports that should not be coloured for
that kind.

See TOPOLOGICAL-SORT")

  (function a*
    "Performs an A* shortest-path calculation.

Returns a list of connections along the shortest
path.

START and GOAL must be nodes of the same graph.
COST-FUN must be a function of two arguments that
returns an estimated cost to move from the first
to the second node that is passed.

Signals an error of type GRAPH-IS-BIPARTITE if no
valid path can be found between START and GOAL.

See GRAPH-IS-BIPARTITE"))

;; nodes.lisp
(docs:define-docs
  (type unit
    "Superclass for all any entity in a Flow graph.

See ATTRIBUTES
See ATTRIBUTE
See REMOVE-ATTRIBUTE
See WITH-ATTRIBUTES")
  
  (function attributes
    "Accessor to the unit's hash table of attributes.

See UNIT
See ATTRIBUTE
See REMOVE-ATTRIBUTE")
  
  (function attribute
    "Accessor to the named attribute on the unit.

The attribute's name must be comparable by EQL.
If the attribute does not exist on the unit, the
default value is returned instead.

See ATTRIBUTES
See REMOVE-ATTRIBUTE
See UNIT")
  
  (function remove-attribute
    "Remove the named attribute from the unit.

See ATTRIBUTES
See ATTRIBUTE
See UNIT")
  
  (function with-attributes
    "Shorthand macro to access the given attributes through a variable.

This is similar to WITH-SLOTS.

See UNIT
See ATTRIBUTE
See CL:WITH-SLOTS")
  
  (type connection
    "Representation of a connection between two ports.

This connection is undirected, meaning that it is
intended to represent information flowing in both
directions.

See LEFT
See RIGHT
See UNIT
See CONNECTION=
See SEVER")
  
  (function left
    "Accessor to the \"left\" port of a connection.

See CONNECTION")
  
  (function right
    "Accessor to the \"right\" port of a connection.

See CONNECTION")
  
  (function connection=
    "Tests whether two connections are considered equal.

Connections are the same under this comparison, if
they are connected to the same ports \"in the same
way\". This simply means that whether ports are
connected the same may depend on the specific
connection being tested. For example, directed
connections are only the same if the left and right
ports match up, whereas undirected connections are
the same regardless of the order between them.

See CONNECTION")
  
  (function sever
    "Sever the connections of this unit.

For a connection, severing it means simply removing
that connection. For a port severing means severing
all connections of the port. For a node severing
severing all connections of all of its ports.

See CONNECTION
See PORT
See NODE")
  
  (type directed-connection
    "A connection for which information only flows from left to right.

See CONNECTION")
  
  (type port
    "Representation of a connection port on a node.

Ports are named places on a node through which
connections between nodes can be made.

See UNIT
See CONNECTIONS
See NODE
See SLOT
See CONNECT
See DISCONNECT
See REMOVE-CONNECTION
See CHECK-CONNECTION-ACCEPTED
See SEVER")
  
  (function connections
    "Accessor to the list of connections on this unit.

The list is not guaranteed to be fresh and thus
may not be modified without potentially messing things
up.

See PORT
See NODE")
  
  (function node
    "Accessor to the node this port is home to.

See PORT")
  
  (function slot
    "Accessor to the name of the slot this port is contained in.

See PORT")
  
  (function connect
    "Forge a connection between the two units.

The connection is only made if it is accepted on both
left and right hand sides by CHECK-CONNECTION-ACCEPTED.
If both accept the connection, it is pushed onto their
respective connections lists.

See PORT
See CHECK-CONNECTION-ACCEPTED
See CONNECTIONS")
  
  (function disconnect
    "Remove any matching connection from left to right.

This constructs a directed-connection between the two
and then removes all connections from each of them that
matches the constructed connection by CONNECTION=.

See PORT
See DIRECTED-CONNECTION
See REMOVE-CONNECTION
See CONNECTION=")
  
  (function remove-connection
    "Remove the given connection from the unit.

See PORT
See NODE
See CONNECTIONS")
  
  (function check-connection-accepted
    "Check whether the given connection is accepted on the given unit.

If it is not accepted, an error is signalled. This
generic function uses a PROGN method combination,
which forces tests of all superclasses to be performed
as well.

See CONNECTION-ALREADY-EXISTS
See ILLEGAL-CONNECTION")
  
  (type n-port
    "A port that accepts an arbitrary number of connections.

See PORT")
  
  (type 1-port
    "A port that only accepts a single connection.

See PORT")
  
  (type in-port
    "A port that only accepts incoming connections.

See PORT")
  
  (type out-port
    "A port that only accepts outgoing connections.

See PORT")
  
  (type node
    "Superclass for all nodes in a Flow graph.

A node has a set of PORT instances that are
used to form connections to other nodes over.

See UNIT
See PORT
See PORTS
See SEVER
See CONNECTIONS
See REMOVE-CONNECTION
See DISCONNECT")
  
  (function ports
    "Returns a list of port objects that the node contains.

This list may not be fresh and thus must not be modified.

See NODE")
  
  (function port
    "Return the port object contained in the node with the specified name.

If the name does not designate a port, an error of type
DESIGNATOR-NOT-A-PORT is signalled.

See NODE
See DESIGNATOR-NOT-A-PORT")

  (type dynamic-node
    "Superclass for all dynamic nodes.

A dynamic node's ports are allocated on a per-instance
basis, rather than on a per-class basis like for the
static-node.

See NODE")

  (function other-node
    "Return the node on the other side of the connection.

This works with both directed and undirected connections.

See TARGET-NODE")

  (function target-node
    "Return the node on the other side of the connection.

If the connection is directed, the target node is only 
returned if the left-side of the connection is the given
node. Otherwise NIL is returned. For undirected connections
this acts the same as OTHER-NODE.

See OTHER-NODE"))

;; static-node.lisp
(docs:define-docs
  (variable *resolve-port*
    "Whether a slot-value/slot-makunbound/slot-boundp call should resolve the port.

If this is T (the default), then the port's
slot within the object's slot is resolved,
rather than directly resolving the slot that the
port is itself contained in.")
    
  (function port-value
    "Accessor to the primary \"value\" contained in this static port.

For standard ports this is the CONNECTIONS slot.

See STATIC-NODE
See PORT-VALUE-BOUNDP
See PORT-VALUE-MAKUNBOUND
See DEFINE-PORT-VALUE-SLOT")

  (function port-value-boundp
    "Returns non-NIL if the value slot in this static port is bound.

See STATIC-NODE
See PORT-VALUE
See PORT-VALUE-MAKUNBOUND
See DEFINE-PORT-VALUE-SLOT")

  (function port-value-makunbound
    "Makes the value slot in this static port unbound.

See STATIC-NODE
See PORT-VALUE
See PORT-VALUE-BOUNDP
See DEFINE-PORT-VALUE-SLOT")

  (function define-port-value-slot
    "Easily define a slot to be used for the port value of a port class.

If ACCESSOR is given it should be a symbol denoting
the name of an accessor responsible for getting and
setting the appropriate value on the port. If it is
not given, SLOT-VALUE is used instead.

This automatically generates appropriate methods for
the port value functions.

See PORT-VALUE
See PORT-VALUE-BOUNDP
See PORT-VALUE-MAKUNBOUND")
    
  (type port-definition
    "Superclass for port definition slot classes.

See PORT-TYPE")
  
  (function port-type
    "Accessor to the port type contained in this slot.

See PORT-DEFINITION")
  
  (type direct-port-definition
    "Class for direct port slot definitions

See PORT-DEFINITION
See C2MOP:STANDARD-DIRECT-SLOT-DEFINITION")
  
  (type effective-port-definition
    "Class for effective port slot definitions

See PORT-DEFINITION
See C2MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION")
  
  (type static-node-class
    "Metaclass for all static nodes.

This class allows the usage of the :PORT-TYPE initarg
on slots. If non-null, the slot is treated as a port
of the node, allowing to be used for connections
between nodes. When such a slot is accessed normally,
it immediately resolves to the PORT-VALUE of the
port contained in the slot.

Every port of a port-typed slot is also automatically
instantiated upon instantiation of the class itself,
ensuring that it is consistent with the definition.

If an access to the actual port object contained in the
slot is necessary, the PORT-SLOT-VALUE and
PORT-SLOT-BOUNDP functions can be used instead.

See PORT-VALUE
See DIRECT-PORT-DEFINITION
See EFFECTIVE-PORT-DEFINITION
See DEFINE-NODE
See PORT-SLOT-VALUE
See PORT-SLOT-BOUNDP")
  
  (function port-slot-value
    "Accessor to the actual port object contained in the node's slot.

See STATIC-NODE
See *RESOLVE-PORT*")
  
  (function port-slot-boundp
    "Test to see whether the actual port object contained in the node's slot is bound.

For any successfully initialised node, this should
always return T.

See STATIC-NODE
See *RESOLVE-PORT*")
  
  (type static-node
    "Superclass for all static nodes.

The set of ports of a static node is defined per-class
and is thus the same for each instance of the class.

In addition to the standard slot keywords, a node
supports the :PORT-TYPE keyword. This takes a symbol
as argument, designating the name of the class to
use for the port of this slot.

If a slot is a port on the class, connections to
other ports may be established through that port.

See NODE
See STATIC-NODE-CLASS
See DEFINE-NODE
See PORTS
See PORT
See SEVER
See CONNECTIONS
See REMOVE-CONNECTION
See DISCONNECT")
  
  (function define-node
    "Shorthand macro to define a static node class.

All this does is add the necessary :METACLASS option
and inject STATIC-NODE as a direct-superclass.

See STATIC-NODE"))

;; toolkit.lisp
(docs:define-docs
  (function find-slots-by-initarg
    "Returns the list of slots that have key as an initarg.")
  
  (function find-slot-by-name
    "Returns the slot whose name matches the given one."))

## About Flow
Flow is a flowchart graph library. Unlike other graphing libraries, this one focuses on nodes in a graph having distinct "ports" through which connections to other nodes are formed. This helps in many concrete scenarios where it is important to distinguish not only which nodes are connected, but also how they are connected to each other.

Particularly, a lot of data flow and exchange problems can be reduced to such a "flowchart". For example, an audio processing library may present its pipeline as a flowchart of segments that communicate with each other through audio sample buffers. Flow gives a convenient view onto this kind of problem, and even allows the generic visualisation of graphs in this format.

## How To
In a Flow graph there's three kinds of `unit`s: `node`s, `port`s, and `connection`s. A node is analogous to a vertex in a graph, a port is analogous to a place where a connection can be made on a node, and a connection is analogous to an edge in a graph.

Of the nodes, there's two kinds: 

* `dynamic-node` A dynamic node's ports are determined at runtime for each individual instance. This is useful for when you're constructing your graph out of elements that you don't know ahead of time.
* `static-node` A static node's ports are determined at class definition time, and each port corresponds to a special kind of slot on the graph. This is usually what you want when you define your graph entirely yourself.

Of the ports, there's several mixin classes that can be used to define the kind of port you want. Naturally, if you want to add extra information you can define your own port classes to use instead.

* `n-port` A port that accepts an arbitrary number of connections.
* `1-port` A port that only accepts a single connection.
* `in-port` A port that only accepts incoming connections.
* `out-port` A port that only accepts outgoing connections.

Of the connections, only two are predefined, though it is easy to imagine situations where other kinds of connections might also come in handy.

* `connection` A basic undirected connection that goes both ways.
* `directed-connection` A directed connection that only goes from left to right.

You can then manage connections between ports using `connect`, `disconnect`, and `sever`. You can also inspect nodes and ports with `ports`, and `connections`.

### A Flow Chart Example
If you wanted to build a classic flow chart library, you could use something like this as your basic building blocks:

    (defclass in (in-port n-port)
      ())
    
    (defclass out (out-port 1-port)
      ())
    
    (define-node start ()
      ((out :port-type out)))
    
    (define-node end ()
      ((in :port-type in)))
    
    (define-node process ()
      ((in :port-type in)
       (out :port-type out)))
    
    (define-node decision ()
      ((in :port-type in)
       (true :port-type out)
       (false :port-type out)))

Using these basic classes we can then create a flow chart like this:

    (let ((start (make-instance 'start))
          (pick-library (make-instance 'process))
          (evaluate-library (make-instance 'process))
          (decide-if-good (make-instance 'decision))
          (end (make-instance 'end)))
      (connect (port start 'out) (port pick-library 'in))
      (connect (port pick-library 'out) (port evaluate-library 'in))
      (connect (port evaluate-library 'out) (port decide-if-good 'in))
      (connect (port decide-if-good 'true) (port end 'in))
      (connect (port decide-if-good 'false) (port pick-library 'in))
      start)

### Operating on Flow Graphs
Flow also includes a couple of operations to help your process the graphs you created using the library. It can do a `topological-sort`, `extract-graph` for you, `color-nodes`, and `allocate-ports`. There's also a generic `visit` to allow you to quickly traverse the graph. See the docstrings of the functions for an in-depth explanation of what they do.

### Visualising a Flow Graph
There is an additional system included called `flow-visualizer`. This system includes a primitive graph visualizer that lets you view and edit a graph directly in a GUI. It isn't very advanced at this point, but will probably be extended in the future to a usable flowchart editor.

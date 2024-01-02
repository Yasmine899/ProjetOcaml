open Graph

(** Returns the number of nodes in a graph. *)
val count_nodes : 'a graph -> int

(** Clones a graph, removing all its arcs. *)
val clone_nodes : 'a graph -> 'a graph

(** Maps all arcs of the graph using the provided function. *)
val map_arcs: 'a graph -> ('a -> 'b) -> 'b graph

(** Adds a value to the label of the arc between id1 and id2. 
 * If the arc doesn't exist, create one. *)
val add_arc : int graph -> id -> id -> int -> int graph

(** Returns all arcs of the graph. *)
val get_all_arcs : 'a graph -> 'a arc list

(** Returns the arcs going to the node with the given id. *)
val incoming_arcs : 'a graph -> id -> 'a arc list

(** Returns a list of arcs from a path (list of nodes). *)
val arcs_of_path : 'a graph -> id list -> 'a arc list

(** Filters the edges of the graph based on a predicate. *)
val filter_edges : 'a graph -> ('a arc -> bool) -> 'a graph

open Graph

val nb_nodes: 'a graph -> int
val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph
val in_arcs: 'a graph -> id -> 'a arc list
val arcs_of_path: 'a graph -> id list -> 'a arc list
val e_filter: 'a graph -> ('a arc -> bool) -> 'a graph
open Graph

val find_path: int graph -> id -> id -> id list option
val min_flow: int graph -> id list -> int
val updating_graph: int graph -> id list -> int -> int graph
val ford_fulkerson: int graph -> id -> id -> (int*int) graph
val arcs_of_path: 'a graph -> id list -> 'a arc list
val e_filter: 'a graph -> ('a arc -> bool) -> 'a graph
val in_arcs: 'a graph -> id -> 'a arc list

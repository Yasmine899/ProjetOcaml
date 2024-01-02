open Graph
open Tools

(** Return an optional path (list of nodes) from s to t. *)
val find_path : 'a graph -> id -> id -> id list option

(** Return the maximal possible flow along the path. *)
val compute_flow : 'a graph -> id list -> int

(** Compute the next graph using the given path and flow. *)
val next_graph : 'a graph -> id list -> int -> 'a graph

(** Compute the Ford-Fulkerson algorithm on the given capacity graph.
    s and t are respectively the source and the sink.
    Return the flow graph with the given capacity. *)
val ford_fulkerson : int graph -> id -> id -> 'a graph

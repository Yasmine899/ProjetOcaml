(* BipartiteMatching.mli *)
open Graph

 val bipartite_matching : int -> int -> int list list -> (id * id) list
val create_bipartite_graph :int -> int -> int list list -> int graph



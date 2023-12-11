open Graph
open Tools



val find_path : int graph -> id -> id -> int list option

val minflot : int graph -> id list -> int

val modify_flot : int graph -> int -> id list -> int graph

val ford_fulkerson : int graph -> id -> id -> string graph 

val print_list : int list option -> unit 
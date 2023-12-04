(* tools.ml *)

open Graph

let clone_nodes (gr: 'a graph) : 'b graph =
  (* Create a new graph with the same nodes but no arcs *)
  List.map (fun (id, _) -> (id, [])) gr

let gmap (gr: 'a graph) (f: 'a -> 'b) : 'b graph =
  (* Map the arcs of the graph using function f *)
  List.map (fun (id, arcs) -> (id, List.map (fun arc -> { arc with lbl = f arc.lbl }) arcs)) gr

let add_arc g id1 id2 n =
    match find_arc g id1 id2 with
    | None -> new_arc g id1 id2 n
    | Some a ->  new_arc g id1 id2 (n+a)



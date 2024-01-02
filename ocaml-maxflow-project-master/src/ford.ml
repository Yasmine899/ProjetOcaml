open Graph
open Tools

(* Return an optional path (list of nodes) from s to t. *)
let find_path g s t = 
  let rec loop path n =
    if List.exists (fun x -> x = n) path 
    then None
    else if n = t
    then Some (n::path)
    else 
      let arcs = out_arcs g n in
      let f op arc =
        match op with
        | Some p -> Some p
        | None -> loop (n::path) arc.tgt
      in
      List.fold_left f None arcs
  in
  let rev_path_opt = loop [] s in
  Option.map List.rev rev_path_opt

(* Return the maximal possible flow along the path. *)
let compute_flow g path =
  let arcs = arcs_of_path g path in
  List.fold_left (fun m arc -> min m arc.lbl) Int.max_int arcs

(* Compute the next graph using the given path and flow. *)
let next_graph g path flow =
  let update_arc accu arc =
    add_arc accu arc.src arc.tgt (-flow)
  in
  let g1 = List.fold_left update_arc g (arcs_of_path g path) in
  let g2 = e_filter g1 (fun arc -> arc.lbl <> 0) in
  let g3 = List.fold_left (fun accu arc -> add_arc accu arc.tgt arc.src flow) g2 (arcs_of_path g path) in
  g3

(* Compute the Ford-Fulkerson algorithm on the given capacity graph.
   s and t are respectively the source and the sink.
   Return the flow graph with the given capacity. *)
let ford_fulkerson graph s t =
  let rec loop g = 
    match find_path g s t with
    | None -> g
    | Some path -> 
      let flow = compute_flow g path in
      let ng = next_graph g path flow in
      loop ng
  in
  let final_graph = loop graph in
  let map_labels g edge =
    let capacity = snd edge.lbl in
    let reverse_arc_opt = find_arc final_graph edge.tgt edge.src in
    let flow = match reverse_arc_opt with
      | None -> 0
      | Some reverse_arc -> reverse_arc.lbl
    in
    new_arc g {edge with lbl = (flow, capacity)}
  in
  e_fold graph map_labels (clone_nodes graph)

open Graph

(* Returns the number of nodes in a graph. *)
let count_nodes g = n_fold g (fun count _ -> count + 1) 0

(* Clones a graph, removing all its arcs. *)
let clone_nodes g = n_fold g new_node empty_graph

(* Maps all arcs of the graph using the provided function. *)
let map_arcs g f = e_fold g (fun acc arc -> new_arc acc {arc with lbl = f arc.lbl}) (clone_nodes g)

(* Adds a value to the label of the arc between id1 and id2. 
* If the arc doesn't exist, create one. *)
let add_arc g id1 id2 value = 
  let new_g = clone_nodes g in
  let update_arc accu arc = 
    if (arc.src, arc.tgt) = (id1, id2) 
    then new_arc accu {arc with lbl = arc.lbl + value} 
    else new_arc accu arc 
  in
  if Option.is_none (find_arc g id1 id2)
  then new_arc g {src = id1; tgt = id2; lbl = value}
  else e_fold g update_arc new_g

(* Returns all arcs of the graph. *)
let get_all_arcs g = e_fold g (fun arcs arc -> arc :: arcs) []

(* Returns the arcs going to the node with the given id. *)
let incoming_arcs g id = List.filter (fun arc -> arc.tgt = id) (get_all_arcs g)

(* Returns a list of arcs from a path (list of nodes). *)
let arcs_of_path graph path =
  let accumulate_arcs acu node =
    let (arcs, prev_node) = acu in
    let arc = Option.get (find_arc graph prev_node node) in
    (arc :: arcs, node)
  in
  let (arcs, _) = List.fold_left accumulate_arcs ([], List.hd path) (List.tl path) in
  arcs

(* Filters the edges of the graph based on a predicate. *)
let filter_edges graph predicate = 
  e_fold graph (fun filtered_edges arc -> if predicate arc then new_arc filtered_edges arc else filtered_edges) (clone_nodes graph)

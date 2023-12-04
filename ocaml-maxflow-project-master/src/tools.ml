open Graph

let clone_nodes (gr: 'a graph) : 'b graph =
  n_fold gr (fun acc id -> new_node acc id) empty_graph

let add_arc g id1 id2 n =
  let new_arc_value = { src = id1; tgt = id2; lbl = n } in
  match find_arc g id1 id2 with
  | None -> new_arc g new_arc_value
  | Some a -> new_arc g { a with lbl = n + a.lbl }

let gmap g f = e_fold g (fun acu e -> new_arc acu {e with lbl = f e.lbl}) (clone_nodes g)



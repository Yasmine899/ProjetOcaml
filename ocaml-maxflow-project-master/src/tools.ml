open Graph

(* Return the number of nodes of a graph. *)
let nb_nodes g = n_fold g (fun r _ -> r + 1) 0

(* Clones a graph, and deletes all his arcs. *)
let clone_nodes g = n_fold g new_node empty_graph

(* Maps all arcs of g by f. *)
let gmap g f = e_fold g (fun acu e -> new_arc acu {e with lbl = f e.lbl}) (clone_nodes g)

(* Adds n to the value of the arc between id1 and id2. 
* If the arc doesn't exist, create one. *)
let add_arc g id1 id2 n = 
  let ge = clone_nodes g in
  let f new_g e = if (e.src,e.tgt) = (id1,id2) 
    then new_arc new_g {e with lbl=e.lbl+n} 
    else new_arc new_g e 
  in
    if Option.is_none (find_arc g id1 id2)
    then new_arc g {src=id1; tgt=id2; lbl=n}
    else e_fold g f ge

(* Return all arcs of the graph. *)
let get_arcs g = e_fold g (fun l e -> e::l) []

(* Return the arcs going to the node with the given id. *)
let in_arcs g id = List.filter (fun e -> e.tgt=id) (get_arcs g)

(* Return a list of arcs from a path (list of nodes). *)
let arcs_of_path graph path =
  let f acu n = 
    let (arcs,prev_n) = acu in
    let arc = Option.get (find_arc graph prev_n n) in
    (arc::arcs,n)
  in
    let (arcs,_) = List.fold_left f ([],List.hd path) (List.tl path) in
    arcs

(* Filter the edges of the graph. *)
let e_filter graph f = 
  e_fold graph (fun g e -> if f e then new_arc g e else g) (clone_nodes graph)
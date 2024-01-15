open Graph



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




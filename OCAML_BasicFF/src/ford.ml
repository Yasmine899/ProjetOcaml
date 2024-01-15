open Graph
open Tools

(* Trouver un chemin entre le nœud source et le nœud cible dans le graphe. *)
let rec explore_path visited graph current target =
  (*Si le nœud actuel est égal à la cible, cela signifie qu'un chemin a été trouvé*)
  if current = target then Some [current] 
 (*si y a un cycle*)
  else if List.mem current visited then None 
    (*sinon*)
  else
    let out_edges = out_arcs graph current in
    (*prend un arc et tente d'explorer le graphe à partir de l'extrémité cible de cet arc.*)
    let try_explore = function
      | { tgt; _ } ->
        match explore_path (current :: visited) graph tgt target with
        | Some path -> Some (current :: path)
        | None -> None
    in
    let rec explore = function
    (*y a plus d arcs sortants*)
      | [] -> None
      (*le cas contraire,on prend un arc*)
      | edge :: rest ->
        match try_explore edge with
        | Some path -> Some path
        | None -> explore rest
    in
    explore out_edges

let find_path graph source target =
  explore_path [] graph source target

(*Calculer le flux minimum le long d'un chemin donné dans le graphe.*)
let min_flow graph path =
  let rec find_min_capacity min_capacity = function
    | [] | [_] -> min_capacity 
    | src :: tgt :: rest ->
      match find_arc graph src tgt with
      | Some arc -> find_min_capacity (min min_capacity arc.lbl) (tgt :: rest)
      | None -> min_capacity
  in
  find_min_capacity max_int path

(* Extraire les arcs correspondant à un chemin donné.*)
  let arcs_of_path graph path =
    let collect_arcs (arcs_accumulator, prev_node) current_node =
      let current_arc = Option.get (find_arc graph prev_node current_node) in
      (current_arc :: arcs_accumulator, current_node)
    in
    let (resulting_arcs, _) = List.fold_left collect_arcs ([], List.hd path) (List.tl path) in
    List.rev resulting_arcs
  
  
(* Filtrer les arcs du graphe en fonction d'un prédicat. *)
let e_filter graph f = 
  e_fold graph (fun g e -> if f e then new_arc g e else g) (clone_nodes graph)

 (* Mettre à jour le graphe en fonction d'un chemin et d'un flux.*)
let updating_graph graph path flow =
  let reverse_flow e = { e with src = e.tgt; tgt = e.src } in
  let reversed_arcs = List.map reverse_flow (arcs_of_path graph path) in
  let updated_graph = List.fold_left (fun g e -> add_arc g e.src e.tgt (-flow)) graph (arcs_of_path graph path) in
  let filtered_graph = e_filter updated_graph (fun e -> e.lbl <> 0) in
  List.fold_left (fun g e -> add_arc g e.src e.tgt flow) filtered_graph reversed_arcs

  (* Récupérer tous les arcs du graphe. *)
let get_arcs g = e_fold g (fun l e -> e::l) []

(* Récupérer les arcs allant vers un nœud donné. *)
let in_arcs g id = List.filter (fun e -> e.tgt=id) (get_arcs g)

  let ford_fulkerson graph source sink =
    let rec loop current_graph =
      match find_path current_graph source sink with
      | None -> current_graph
      | Some path ->
        Printf.printf "Path found: %s\n" (String.concat " " (List.map string_of_int path));
        let flow = min_flow current_graph path in
        let updated_graph = updating_graph current_graph path flow in
        loop updated_graph
    in
  
    let final_graph = loop graph in
  (*Mettre à jour le flux d'un arc dans le graphe final.*)
    let update_flow g e =
      let capacity = e.lbl in
      let r_arc_opt = find_arc final_graph e.tgt e.src in
      let flow = match r_arc_opt with
        | None -> 0
        | Some r_arc -> min capacity r_arc.lbl
      in
      new_arc g {e with lbl = (flow, capacity)}
    in
  
    e_fold graph update_flow (clone_nodes graph)
  

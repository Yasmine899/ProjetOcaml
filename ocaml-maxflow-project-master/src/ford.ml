open Graph
open Tools


let rec explore_path visited graph current target =
  if current = target then Some [current]
  else if List.mem current visited then None
  else
    let out_edges = out_arcs graph current in
    let try_explore = function
      | { tgt; _ } ->
        match explore_path (current :: visited) graph tgt target with
        | Some path -> Some (current :: path)
        | None -> None
    in
    let rec explore = function
      | [] -> None
      | edge :: rest ->
        match try_explore edge with
        | Some path -> Some path
        | None -> explore rest
    in
    explore out_edges

let find_path graph source target =
  explore_path [] graph source target


let min_flow graph path =
  let rec find_min_capacity min_capacity = function
    | [] | [_] -> min_capacity 
    | src :: tgt :: rest ->
      match find_arc graph src tgt with
      | Some arc -> find_min_capacity (min min_capacity arc.lbl) (tgt :: rest)
      | None -> min_capacity
  in
  find_min_capacity max_int path


let updating_graph graph path flow =
  let reverse_flow e = { e with src = e.tgt; tgt = e.src } in
  let reversed_arcs = List.map reverse_flow (arcs_of_path graph path) in
  let updated_graph = List.fold_left (fun g e -> add_arc g e.src e.tgt (-flow)) graph (arcs_of_path graph path) in
  let filtered_graph = e_filter updated_graph (fun e -> e.lbl <> 0) in
  List.fold_left (fun g e -> add_arc g e.src e.tgt flow) filtered_graph reversed_arcs


let ford_fulkerson graph s t =
  let rec loop g = 
    let opt_path = find_path g s t in 
    match opt_path with
      | None -> g
      | Some path -> 
      Printf.printf "Path found: ";
      List.iter (fun n -> Printf.printf "%d " n) path;
      Printf.printf "\n";
        let flow = min_flow g path in
        let ng = updating_graph g path flow in
          loop ng
    in
      let final_graph = loop graph in
      let func g e =  
        let capacity = e.lbl in
        let r_arc_opt = find_arc final_graph e.tgt e.src in
        let flow = match r_arc_opt with
          | None -> 0
          | Some r_arc -> min capacity r_arc.lbl
        in
          new_arc g {e with lbl=(flow,capacity)}
      in
        e_fold graph func (clone_nodes graph)


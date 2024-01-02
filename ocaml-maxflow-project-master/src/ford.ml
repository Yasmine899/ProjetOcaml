open Graph
open Tools
(* open Gfile *)

(* Return the max flow and its path from s to t. *)
(*
let find_path g s t =
  let table = Hashtbl.create (nb_nodes g) in
  let () = Hashtbl.add table t (Int.max_int,[t]) in
  let rec loop n = 
    let (flow,path) = Hashtbl.find table n in
    let explore e = 
      (* If we have already explore the source. *)
      if Hashtbl.mem table e.src 
      then 
        let (flow_src,_) = Hashtbl.find table e.src in 
        let new_flow = min flow e.lbl in
        if new_flow > flow_src then
          Hashtbl.replace table e.src (new_flow, e.src::path);
          loop e.src
      else 
        Hashtbl.add table e.src (min flow e.lbl, e.src::path);
        loop e.src

    in 
      List.iter explore (in_arcs g n)
  in
    loop t;
    Hashtbl.find_opt table s*)

(* path(X,Y) :- arc(X,Y) *)
(* path(X,Z) :- arc(X,Y), path(Y,Z) *)

(* Return an optional path (list of nodes) from s to t. *)
let find_path g s t = 
  let rec loop path n =
    if List.exists (fun x -> x = n) path 
      then None
      else 
        if n = t
        then Some (n::path)
        else 
          let arcs = out_arcs g n in
          let f op a = match op with
            | Some p -> Some p
            | None -> loop (n::path) a.tgt
          in
            List.fold_left f None arcs
  in
    let rev_path_opt = loop [] s in
    Option.map List.rev rev_path_opt


(* Return the maximal possible flow along the path. *)
let compute_flow g path =
  let arcs = arcs_of_path g path in
  List.fold_left (fun m e -> min m e.lbl) Int.max_int arcs

(* Compute the next graph using the given path and flow. *)
let next_graph g path flow =
  let arcs = arcs_of_path g path in
  let g1 = List.fold_left (fun g e -> add_arc g e.src e.tgt (-flow)) g arcs in
  let g2 = e_filter g1 (fun e -> e.lbl <> 0) in
  let g3 = List.fold_left (fun g e -> add_arc g e.tgt e.src flow) g2 arcs in
  g3

(* Compute the Ford-Fulkerson algorithm on the given capacity graph.
  s and t are respectively the source and the sink.
  Return the the flow graph with the given capacity. *)
let ford_fulkerson graph s t =
  let rec loop g = 
    let opt_path = find_path g s t in 
    match opt_path with
      | None -> g
      | Some path -> 
        let flow = compute_flow g path in
        let ng = next_graph g path flow in
          loop ng
    in
      let final_graph = loop graph in
      (* export (gmap final_graph string_of_int); *)
      let func g e =  
        let capacity = e.lbl in
        let r_arc_opt = find_arc final_graph e.tgt e.src in
        let flow = match r_arc_opt with
          | None -> 0
          | Some r_arc -> r_arc.lbl
        in
          new_arc g {e with lbl=(flow,capacity)}
      in
        e_fold graph func (clone_nodes graph)

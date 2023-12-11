open Graph
open Tools
open Gfile

let find_path graph id1 id2 =
  let rec pl id1 id2 visited =
    if id1 = id2 then
      Some [id2]
    else if List.mem id1 visited then
      None
    else
      match List.assoc id1 graph with
      | arcs ->
        let try_path (tgt, _) = pl tgt id2 (id1 :: visited) in
        match List.find_opt (fun (_, lbl) -> lbl > 0) arcs with
        | Some (next_id, _) ->
          Option.map (fun path -> id1 :: path) (try_path next_id)
        | None -> None
  in
  match pl id1 id2 [] with
  | Some path -> Some (List.rev path)
  | None -> None


  let minflot gr path =
    match path with
    | None -> failwith "no path found"
    | Some path_list ->
      let rec find_min_capacity min_cap gr path_list =
        match path_list with
        | [] -> min_cap
        | [_] -> min_cap
        | id1 :: id2 :: _ ->
          let arcs = find_arc gr id1 id2 in
          match arcs with
          | None -> failwith "arc not found"
          | Some arc -> find_min_capacity (min min_cap arc.lbl) gr (List.tl path_list)
      in
      find_min_capacity max_int gr path_list;;
  

      let modify_flot gr capacity path_list =
        let rec update_arc_capacity g path cap =
          match path with
          | [] | [_] -> g
          | id1 :: id2 :: tl ->
            let up_gr =
              match find_arc g id1 id2 with
              | None -> failwith "arc not found"
              | Some arc -> add_arc g id1 id2 (arc.lbl - cap) 
            in
            update_arc_capacity up_gr (id2 :: tl) cap
        in
        update_arc_capacity gr path_list capacity;;
      

let print_list = function
  | None -> Printf.printf "no path found\n"
  | Some l -> Printf.printf "Path: [%s]\n" (String.concat " -> " (List.map string_of_int l))

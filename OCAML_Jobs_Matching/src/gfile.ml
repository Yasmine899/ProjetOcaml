open Graph
open Printf
    
type path = string

(* Format of text files:
   % This is a comment

   % A node with its coordinates (which are not used), and its id.
   n 88.8 209.7 0
   n 408.9 183.0 1

   % Edges: e source dest label id  (the edge id is not used).
   e 3 1 11 0 
   e 0 2 8 1

*)

(* Compute arbitrary position for a node. Center is 300,300 *)
let iof = int_of_float
let foi = float_of_int

let index_i id = iof (sqrt (foi id *. 1.1))

let compute_x id = 20 + 180 * index_i id

let compute_y id =
  let i0 = index_i id in
  let delta = id - (i0 * i0 * 10 / 11) in
  let sgn = if delta mod 2 = 0 then -1 else 1 in

  300 + sgn * (delta / 2) * 100
  
(*
let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% This is a graph.\n\n" ;

  (* Write all nodes (with fake coordinates) *)
  n_iter_sorted graph (fun id -> fprintf ff "n %d %d %d\n" (compute_x id) (compute_y id) id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  let _ = e_fold graph (fun count arc -> fprintf ff "e %d %d %d %s\n" arc.src arc.tgt count arc.lbl ; count + 1) 0 in
  
  fprintf ff "\n%% End of graph\n" ;
  
  close_out ff ;
  ()

*)
  type data =
  { employees: string list ;
    jobs: string list ;
    preferences: (string * string) list }

(* Add an element to a list *)
let addlist lst a = lst @ [a]

(* Read and process a line with employee information *)
let read_employees (prob :data) line= try 
Scanf.sscanf line " e %s "(fun name ->  { prob with employees = addlist prob.employees name});
with e ->
 Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
 failwith "from_file"

(* Read and process a line with job information *)
let read_jobs (prob :data) line= try 
Scanf.sscanf line " j %s " (fun  job_name ->  { prob with jobs = addlist prob.jobs job_name} );
with e ->
 Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
 failwith "from_file"

 (* Read and process a line with preferences information *)
let read_pref (prob :data) line= try 
Scanf.sscanf line " v %s %s "(fun name voeu->  { prob with preferences = addlist prob.preferences (name,voeu)});
with e ->
 Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
 failwith "from_file"


 let rec iter_list list n i  =  match list with 
    |[] -> i
    |id:: tail -> if id=n then i else iter_list tail n (i+1)

 (* Assign an ID to a node (employee or job) based on its type and position in the list *)
 let node_id (node_name: string) (recu : data)  = match List.exists (fun i -> i=node_name) recu.employees with 
  |true ->  iter_list recu.employees node_name 2
  |false -> let len= List.length recu.employees in 
  iter_list recu.jobs node_name (len+2)

(* Create a list associating employees to their IDs *)
let  id_employee ( recu : data ) =
  let rec aux liste= match liste with 
  |[] -> []
  |x:: rest ->  (x,(node_id x recu)) :: aux rest
in 
aux recu.employees

(* Create a list associating jobs to their IDs *)
let id_jobs (recu : data) = 
  let rec aux liste=match liste with 
  |[] -> []
  |x :: rest -> (x,(node_id x recu )) :: aux rest 
in 
aux recu.jobs

(* Combine lists of employee and job IDs *)
let employee_jobs_list prob = List.append (id_employee prob) (id_jobs prob)

(* Create a source node in the graph *)
let source_node gr = (new_node gr 0)

(* Create a destination node in the graph *)
let destination_node gr =   (new_node gr 1)

(* Connect source node to employees in the graph *)
let src_employee data gr =
  let rec aux gr1 liste =
    match liste with
    | [] -> gr1
    | (_, y) :: rest ->
      let arc = {src = 0; tgt = y; lbl = 1} in
      let new_gr = new_arc gr1 arc in
      aux new_gr rest
  in
  aux gr (id_employee data)

(* Connect jobs to the destination node in the graph *)
let jobs_tgt data gr =
  let rec aux liste gr1 =
    match liste with
    | [] -> gr1
    | (_, y) :: rest ->
      let arc = {src = y; tgt = 1; lbl = 1} in
      let new_gr = new_arc gr1 arc in
      aux rest new_gr
  in 
  aux (id_jobs data) gr



(* Create nodes in the graph based on employee and job information *)
let  read_node graph prob = 
  let rec aux graph1 liste = match liste with 
|[] -> graph1
|(_,y) :: rest -> aux (new_node graph1 y) rest 
in 
aux graph (employee_jobs_list prob)

let read_arc graph data =
  let rec aux graph1 liste =
    match liste with
    | [] -> graph1
    | (x, y) :: rest ->
      let x1 = List.assoc x (employee_jobs_list data) in
      let y1 = List.assoc y (employee_jobs_list data) in
      let arc = {src = x1; tgt = y1; lbl = 1} in
      let new_graph = new_arc graph1 arc in
      aux new_graph rest
  in
  aux graph data.preferences


(* Reads a line with a node. *)
(*let read_node graph line =
  try Scanf.sscanf line "n %f %f %d" (fun _ _ id -> new_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"*)

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
(*let ensure graph id = if node_exists graph id then graph else new_node graph id*)

let export path graph =
let ff = open_out path in
fprintf ff "digraph MyGraph {\n" ;
fprintf ff "fontname=\"Helvetica,Arial,sans-serif\"\n" ;
fprintf ff "node [fontname=\"Helvetica,Arial,sans-serif\"]\n" ;
fprintf ff "edge [fontname=\"Helvetica,Arial,sans-serif\"]\n" ;
fprintf ff "rankdir=LR;\n" ;
fprintf ff "node [shape = circle];\n" ;

(* Write all arcs *)

e_iter graph (fun arc -> fprintf ff "\t%d -> %d [ label = \"%s\" ];\n" arc.src arc.tgt arc.lbl) ;
fprintf ff "}\n" ;

close_out ff ;
()


(* Reads a line with an arc. *)
(*let read_arc graph line =
  try Scanf.sscanf line "e %d %d %_d %s@%%"
        (fun src tgt lbl -> let lbl = String.trim lbl in new_arc (ensure (ensure graph src) tgt) { src ; tgt ; lbl } )
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"*)

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"
(*
let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'n' -> read_node graph line
          | 'e' -> read_arc graph line

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line
      in      
      loop graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop empty_graph in
  
  close_in infile ;
  final_graph*)
  let from_file path =
    let infile = open_in path in 
  
    let rec loop data1 =
      try
        let line = input_line infile in 
  
        let line = String.trim line in
  
        let data2 =
          if line = "" then data1
  
          else match line.[0] with
          |'e' -> read_employees data1 line
          |'j' -> read_jobs data1 line
          |'v' -> read_pref data1 line
          | _ -> read_comment data1 line
        in 
        loop data2
      with End_of_file -> data1
    in
  
    let data3 =
    { employees = [] ;
      jobs = [];
      preferences = [] } in 
  
    let final_data1 = loop data3 in
    close_in infile ;
    final_data1  
  
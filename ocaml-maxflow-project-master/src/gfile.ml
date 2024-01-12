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
  type problem =
  { employees: string list ;
    jobs: string list ;
    voeux: (string * string) list }

(* let node_id (node_name: string) -> int *)

let append_item lst a = lst @ [a]
(*read_ff_e est une fonction qui permet de lire une ligne dans le fichier qui commence par e (employé) et la rajoute dans liste d'employés du type problème*)
let read_ff_e (prob :problem) line= try 
Scanf.sscanf line " e %s "(fun name ->  { prob with employees = append_item prob.employees name});
with e ->
 Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
 failwith "from_file"
(*cette fonction est comme celle d'avant mais elle rajoute les éléments à la liste des jobs dans le type problème*)
let read_ff_j (prob :problem) line= try 
Scanf.sscanf line " j %s " (fun  job_name ->  { prob with jobs = append_item prob.jobs job_name} );
with e ->
 Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
 failwith "from_file"
(*elle fait pareil mais avec une liste de voeux *)
let read_ff_v (prob :problem) line= try 
Scanf.sscanf line " v %s %s "(fun name voeu->  { prob with voeux = append_item prob.voeux (name,voeu)});
with e ->
 Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
 failwith "from_file"

(* le fonction iter_list n'est qu'une fonction intermédiaire qui permet de parcourir une liste, et en fonction de leurs positions attributs un id aux éléments de la liste *)

 let rec iter_list list n i  =  match list with 
    |[] -> i
    |id:: tail -> if id=n then i else iter_list tail n (i+1)
 
    (*cette fonction attribut aux éléments des deux listes d'employés et de jobs différents ids (elle va ensuite être utilisée dans la création du graphe )*)
 let node_id (node_name: string) (recu : problem)  = match List.exists (fun i -> i=node_name) recu.employees with 
  |true ->  iter_list recu.employees node_name 2
  |false -> let len= List.length recu.employees in 
  iter_list recu.jobs node_name (len+2)

(*********crer le dictionnaire qui aura l'association entre name and id ********)

(* cette fonction créer un dictionnaire avec la listé d'employés en attribuant à chaque employant un id (le premier aura l'id 2 puisque les id 0 et 1 sont déjà pris par la source et la destination)*)
let  diction1 ( recu : problem ) =
  let rec aux liste= match liste with 
  |[] -> []
  |x:: rest ->  (x,(node_id x recu)) :: aux rest
in 
aux recu.employees

(*pareil que diction1 mais elle le fait avec la liste des jobs *)
let diction2 (recu : problem) = 
  let rec aux liste=match liste with 
  |[] -> []
  |x :: rest -> (x,(node_id x recu )) :: aux rest 
in 
aux recu.jobs

(* diction permet d'avoir la liste complète des noeuds avec leurs ids *)
let diction prob = List.append (diction1 prob) (diction2 prob)

(* on crée le snodes source et destination*)
let source0 gr = (new_node gr 0)
let dest1 gr =   (new_node gr 1)

(*cette fonction sera utilisée pour lié la source avec les employés*)
let extremite0 problem gr =
  let rec aux gr1 liste =
    match liste with
    | [] -> gr1
    | (_, y) :: rest ->
      let arc = {src = 0; tgt = y; lbl = 1} in
      let new_gr = new_arc gr1 arc in
      aux new_gr rest
  in
  aux gr (diction1 problem)


(*cette fonction sera utilisée pour lier la source avec les jobs *)
let extremite1 problem gr =
  let rec aux liste gr1 =
    match liste with
    | [] -> gr1
    | (_, y) :: rest ->
      let arc = {src = y; tgt = 1; lbl = 1} in
      let new_gr = new_arc gr1 arc in
      aux rest new_gr
  in 
  aux (diction2 problem) gr



(**********creer le graphe à partir de notre problème  **************)

(*cette fonction prend un problème en entrée et crée les noeuds en fonction de ses listes *)
let  read_node graph prob = 
  let rec aux graph1 liste = match liste with 
|[] -> graph1
|(_,y) :: rest -> aux (new_node graph1 y) rest 
in 
aux graph (diction prob)

(*cette fonction prend un problème en entrée et crée des arcs en fonction de la liste des voeux *)
let read_arc graph problem =
  let rec aux graph1 liste =
    match liste with
    | [] -> graph1
    | (x, y) :: rest ->
      let x1 = List.assoc x (diction problem) in
      let y1 = List.assoc y (diction problem) in
      (* Printf.printf "%d %d\n" y1 x1; *)
      let arc = {src = x1; tgt = y1; lbl = 1} in
      let new_graph = new_arc graph1 arc in
      aux new_graph rest
  in
  aux graph problem.voeux


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
  
    let rec loop problem1 =
      try
        let line = input_line infile in 
  
        let line = String.trim line in
  
        let problem2 =
          (* Ignore empty lines *)
          if line = "" then problem1
  
          (* The first character of a line determines its content : n or e. *)
          else match line.[0] with
          |'e' -> read_ff_e problem1 line
          |'j' -> read_ff_j problem1 line
          |'v' -> read_ff_v problem1 line
          | _ -> read_comment problem1 line
        in 
        loop problem2
      with End_of_file -> problem1
    in
  
    let problem3 =
    { employees = [] ;
      jobs = [];
      voeux = [] } in 
  
    let final_problem1 = loop problem3 in
    close_in infile ;
    final_problem1  
  

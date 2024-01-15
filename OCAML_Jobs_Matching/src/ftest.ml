open Gfile
open Tools
open Ford
open Graph
(*open Bipartite*)

let () =
  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 6 then
    begin
      Printf.printf "%i" (Array.length Sys.argv);
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;

  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  let infile = Sys.argv.(1) in
  let source = int_of_string Sys.argv.(2) in
  let sink = int_of_string Sys.argv.(3) in
 (* let outfile = Sys.argv.(4) in*)
  let outfiledot = Sys.argv.(5) in

  (* Open file *)
  (*let matching_result = bipartite_matching num_applicants num_jobs preferences in*)
  (*
  let graph = from_file infile in
  let graph = gmap graph int_of_string in
  let final_graph = ford_fulkerson graph source sink in 
  let str_graph = gmap final_graph (fun (x,y) -> Printf.sprintf "(%d,%d)" x y) in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile str_graph in
  ();

  export outfiledot str_graph;*)
  (* Lecture des données à partir du fichier *)
let data = from_file infile in 

(* Création du nœud source dans le graphe initial *)
let s = source_node empty_graph in

(* Création du nœud destination connecté au nœud source *)
let d = destination_node s in

(* Relier le nœud source à tous les employés du graphe *)
let graph1 = src_employee data d in

(* Ajouter les nœuds du graphe basés sur les informations sur les employés et les emplois *)
let graph2 = read_node graph1 data in

(* Ajouter les arcs du graphe basés sur les préférences *)
let graph3 = read_arc graph2 data in 

(* Connecter le nœud source à tous les jobs du graphe *)
let graph4 = jobs_tgt data graph3 in

(* Appliquer l'algorithme de Ford-Fulkerson pour résoudre le problème de flux maximal *)
let final_graph = ford_fulkerson graph4 source sink in 

(* Créer une représentation textuelle du graphe final *)
let str_graph = gmap final_graph (fun (x, y) -> Printf.sprintf "(%d,%d)" x y) in
  (* Rewrite the graph that has been read. *)
  (*let () = write_file outfile str_graph in*)
  ();

  export outfiledot str_graph;
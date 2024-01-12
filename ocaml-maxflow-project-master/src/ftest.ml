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
  let problem = from_file infile in 
  let s = source0 empty_graph in
  let d = dest1 s in
  (*on cree le noeud source et on le relie a tous les noeuds*)
  let graph1= extremite0 problem d in
  let graph2 = read_node graph1 problem in
  let graph3 = read_arc graph2 problem in 
  (*on cree le noeud source et on le relie a tous les noeuds*)
  let graph4 = extremite1 problem graph3 in

  let final_graph = ford_fulkerson graph4 source sink in 
  let str_graph = gmap final_graph (fun (x,y) -> Printf.sprintf "(%d,%d)" x y) in

  (* Rewrite the graph that has been read. *)
  (*let () = write_file outfile str_graph in*)
  ();

  export outfiledot str_graph;
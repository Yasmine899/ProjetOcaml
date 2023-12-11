open Gfile
open Tools
open Ford

let () =
  if Array.length Sys.argv <> 6 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile outfiledot\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written\n" ^
         "    🟄  outfiledot : output file in which the dot result should be written.\n\n") ;
      exit 0
    end ;

  let infile = Sys.argv.(1) in
  let outfiledot = Sys.argv.(5) in
  let _source = int_of_string Sys.argv.(2) in
  let _sink = int_of_string Sys.argv.(3) in
  let outfile = Sys.argv.(4) in

  let graph = from_file infile in

  let sgraph =  gmap graph int_of_string in

  let path_result = find_path sgraph _source _sink in
  Printf.printf "Find Path Result: %s\n"
    (match path_result with
     | None -> "None"
     | Some path -> String.concat " -> " (List.map string_of_int path));

  let minflot_result =
    match path_result with
    | None -> failwith "No path found."
    | Some path_list -> minflot sgraph path_list
  in
  Printf.printf "Minflot Result: %d\n" minflot_result;

  let modified_graph_result =
    match path_result with
    | None -> sgraph
    | Some path_list -> modify_flot sgraph minflot_result path_list
  in

  let modified_graph_str = gmap modified_graph_result (fun lbl -> string_of_int lbl) in
  write_file outfile modified_graph_str;

  export outfiledot modified_graph_str;
  ()

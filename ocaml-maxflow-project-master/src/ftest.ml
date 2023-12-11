
open Gfile
open Tools

let () =
  if Array.length Sys.argv <> 6 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;

  let infile = Sys.argv.(1) in
let outfiledot=Sys.argv.(5)in
  let _source = int_of_string Sys.argv.(2) in
  let _sink = int_of_string Sys.argv.(3) in
  let outfile = Sys.argv.(4) in

  let graph = from_file infile in



  let graph2 = gmap graph (fun lbl -> int_of_string lbl) in
  let graph3 = add_arc graph2 0 5 15 in
  let graph4 = gmap graph3 (fun lbl -> string_of_int lbl) in

  write_file outfile graph4;

let ()=write_file outfile graph4 in
let ()=export outfiledot graph4 in
  ()



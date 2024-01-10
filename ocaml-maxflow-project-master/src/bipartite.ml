open Graph
open Tools
open Ford
(*demarche a suivre
create_bipartite_graph :

    Cette fonction crée un graphe biparti à partir du nombre d'applicants, de jobs et des préférences donnés.
    Elle ajoute des nœuds pour les applicants et les jobs dans le graphe.
    Elle ajoute des arcs correspondant aux préférences des applicants pour les jobs en utilisant la capacité 1 pour chaque arc.

bipartite_matching :

    Cette fonction utilise l'algorithme de Ford-Fulkerson pour résoudre le matching dans le graphe biparti.
    Elle crée d'abord le graphe biparti à l'aide de create_bipartite_graph.
    Ensuite, elle applique l'algorithme de Ford-Fulkerson sur ce graphe pour trouver les correspondances entre les applicants et les jobs.
    Elle parcourt le graphe résolu pour trouver les correspondances :
        Pour chaque applicant, elle récupère les arcs sortants.
        Elle examine ces arcs pour trouver ceux avec un flux de 0 (qui représentent les correspondances) et les stocke dans la liste matching.*)


(* graphe creation from pref *)
let create_bipartite_graph num_applicants num_jobs preferences =
  let graph_ref : 'a graph ref = ref empty_graph in 

  (* Add nodes for applicants *)
  for i = 1 to num_applicants do
    graph_ref := new_node !graph_ref i;
  done;

  (* Add nodes for jobs *)
  for j = 1 to num_jobs do
    graph_ref := new_node !graph_ref (num_applicants + j);
  done;

  (* Add arcs corresponding to applicants' preferences for jobs *)
  List.iteri (fun i prefs ->
    List.iter (fun j ->
      graph_ref := add_arc !graph_ref i (num_applicants + j) 1 (* capacite *)
    ) prefs
  ) preferences;

  !graph_ref

(* Résolution du bipartite matching en utilisant l'algorithme de Ford-Fulkerson *)
(*matching -> stocker les correspondances trouvées entre les candidats et les emplois.*)
let bipartite_matching num_applicants num_jobs preferences : (id * id) list =
  let graph = create_bipartite_graph num_applicants num_jobs preferences in
  let matching_graph = ford_fulkerson graph 0 (num_applicants + num_jobs + 1) in
  let matching = ref [] in
  for i = 1 to num_applicants do
    let out_edges = out_arcs matching_graph i in
    List.iter (fun arc ->
      match arc.lbl with
      | (0, _) -> 
        matching := (arc.src, arc.tgt - num_applicants) :: !matching
      | _ -> ()
    ) out_edges
  done;

  !matching



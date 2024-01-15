# Objectif du Projet

Le projet se divise en deux parties distinctes, chacune se concentrant sur des aspects spécifiques des graphes.
Partie 1 : Implémentation de l'algorithme de Ford-Fulkerson

L'objectif principal de la première partie du projet est d'implémenter l'algorithme de Ford-Fulkerson en OCaml pour résoudre le problème du flux maximal dans un graphe. L'algorithme est utilisé pour trouver le flux maximal à travers un réseau de flux, en particulier dans le contexte de réseaux de transport ou de flots.
Fonctions Principales

    explore_path : Fonction récursive qui explore le graphe à partir d'un nœud donné pour trouver un chemin entre deux nœuds spécifiés.

    find_path : Fonction enveloppante qui utilise explore_path pour trouver un chemin entre le nœud source et le nœud cible dans le graphe.

    min_flow : Calcule le flux minimal le long d'un chemin donné dans le graphe.

    arcs_of_path : Extrait les arcs correspondant à un chemin donné dans le graphe.

    e_filter : Filtrage des arcs du graphe en fonction d'un prédicat.

    updating_graph : Mise à jour du graphe en fonction d'un chemin et d'un flux.

    get_arcs : Récupération de tous les arcs du graphe.

    in_arcs : Récupération des arcs allant vers un nœud spécifié dans le graphe.

    ford_fulkerson : Application de l'algorithme de Ford-Fulkerson pour résoudre le problème du flux maximal dans le graphe.

    update_flow : Mise à jour du flux d'un arc dans le graphe final après l'exécution de l'algorithme.
# Partie 2 : Matching Bipartite

La deuxième partie du projet concerne le problème du "Matching Bipartite" implémenté dans le fichier matching.ml. Cette partie traite de la création d'un graphe bipartite représentant les préférences entre un ensemble de postulants et d'emplois, suivi de l'application de l'algorithme de Ford-Fulkerson pour trouver un appariement optimal.

Compilation du Code

Le code est écrit en OCaml et peut être compilé à l'aide de make demo.

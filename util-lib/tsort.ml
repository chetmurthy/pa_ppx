
(* Stable Tsort
 *
 * We have two datastructures we use for the algorithm:
 *
 * (a) [todo]: this holds a list of zero-indegree vertexes
 *
 * (b) [indegree]: this is a map from vertex to indegree, for vertexes of NONZERO indegree
 *
 * If at any time, indegree is not empty, but todo is empty, we have a failure (b/c there's a cycle)
 *
 * Algorithm:
 *
 * (1) basis case: iterate across vertexes, populating [todo] and [indegree]
 *
 * (2) step case:
 *     (a) grab [todo] list as [t]
 *     (b) zero [todo] list
 *     (c) for each vertex [v] in [t], decrement [indegree v]
 *         if [indegree v] is zero, delete from [indegree] and insert into [todo]
 *     (d) sort [t]
 *     (e) deliver [v] in [t] to [f]
 *
 *)

open Coll
open Std
open Std2

let uniquize ?(compare=Stdlib.compare) l =
  let l = List.sort compare l in
  let rec urec  = function
    | acc,[] -> acc
    | [], h::t -> urec ([h], t)
    | (h1::_ as acc), (h2::t) ->
      if 0 == compare h1 h2 then urec (acc, t)
      else urec (h2::acc, t)
  in
  urec ([], l)

(* a graph as an adjacency list *)
type 'a graph_t = ('a * 'a list) list

let nodes (succadj : 'a graph_t) =
  let acc = ref [] in
  List.iter (fun (s,dlist) -> push acc s; List.iter (push acc) dlist) succadj ;
  uniquize ~compare !acc

let invert_adj ?(compare=Stdlib.compare) nodes succadj =
  let pred = Hashtbl.create 97 in
  List.iter (fun (s,dlist) ->
      List.iter (fun d ->
          Hashtbl.add pred d s)
        dlist)
    succadj ;
  let acc = ref [] in
  List.iter (fun n ->
      if Hashtbl.mem pred n then
        push acc (n, Hashtbl.find_all pred n)) nodes ;
  List.sort (fun (n, _) (m, _) -> compare n m) !acc

let succ2hash nodes (succadj : 'a graph_t) =
  let succ = Hashtbl.create 97 in
  List.iter (fun n -> Hashtbl.add succ n []) nodes ;
  List.iter (fun (s,dlist) -> Hashtbl.replace succ s dlist) succadj ;
  succ

let make_indegree nodes (succadj : 'a graph_t) =
  let indegree = Hashtbl.create 97 in
  List.iter (fun v -> Hashtbl.add indegree v 0) nodes ;
  List.iter (fun (s, dlist) ->
      List.iter (fun d ->
          Hashtbl.replace indegree d (1 + Hashtbl.find indegree d))
        dlist
    )
    succadj ;
  indegree

let canon (l : 'a list) = List.sort compare l

let tsort ?(compare=Stdlib.compare) ?(invertadj=false) f (succadj : 'a graph_t) acc =
  let nodes = nodes succadj in

  let adj =
    if invertadj then
       (invert_adj nodes succadj)
    else
      succadj in

  let adjhash = succ2hash nodes adj in

  let indegree = make_indegree nodes adj in

  let todo = ref [] in

  List.iter (fun v ->
      if Hashtbl.find indegree v = 0 then begin
        push todo v ;
        Hashtbl.remove indegree v
      end
    )
    nodes ;

  let rec dorec acc =
    if !todo = [] then
      if Hashtbl.length indegree = 0 then acc
      else failwith "tsort: DAG was cyclic!"
    else
    let work = !todo in
    todo := [] ;
    List.iter (fun src ->
        let dlist = Hashtbl.find adjhash src in
        List.iter (fun dst ->
            assert (Hashtbl.mem indegree dst) ;
            let d = Hashtbl.find indegree dst in
            if d = 1 then begin
                Hashtbl.remove indegree dst ;
                push todo dst
              end
            else
              Hashtbl.replace indegree dst (d-1)
          ) dlist
      ) work ;
    let work = canon work in
    let acc = List.fold_left (fun acc v -> f v acc) acc work in
    dorec acc

  in dorec acc

let cycles (succadj : 'a graph_t) =
  let edge_adj =
    succadj
    |> List.map (fun (s, dl) -> List.map (fun d -> (s,d)) dl)
    |> List.concat in
  let nodes = hash_uniq ((List.map fst edge_adj) @ (List.map snd edge_adj)) in
  let hadj = Tsort0.mkadj edge_adj in
  Tsort0.cycles nodes hadj

let collapse_cycles (succadj : 'a graph_t) =
  let _, cyl = cycles succadj in
  let cymap =
    cyl
    |> List.map (fun (rep, nl) ->
        List.map (fun n -> (n,rep)) nl)
  |> List.concat in
  let cymap = MHM.ofList 23 cymap in
  let rename n =
    if MHM.in_dom cymap n then MHM.map cymap n else n in
  let collapsed_succadj =
    succadj
    |> List.map (fun (s, dl) -> (rename s, List.map rename dl))
    |> nway_partition (fun (s1, _) (s2, _) -> s1=s2)
    |> List.map (function
        | [] -> assert false
        | (s, dl)::l ->
          let dl = dl@(List.concat (List.map snd l)) in
          let dl = hash_uniq dl in
          let dl = except s dl in
          (s, dl)
      )
  in cyl, collapsed_succadj

let tsort_cyclic ?(compare=Stdlib.compare) ?(invertadj=false) (succadj : 'a graph_t) =
  let cyl, collapsed = collapse_cycles succadj in
  let cyl = List.map (fun (rep, nl) -> (rep, List.sort compare nl)) cyl in
  let order = tsort ~compare ~invertadj (fun v l -> v::l) collapsed [] in
  let cymap = MHM.ofList 23 cyl in
  List.map (fun n ->
      if MHM.in_dom cymap n then MHM.map cymap n else [n]) order

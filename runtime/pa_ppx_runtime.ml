(* camlp5r *)
(* pa_ppx_runtime.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

module Stdlib = Stdlib ;

module Fmt = Fmt ;

value rec map_bind f acc xs =
  let open Rresult.R in 
  match xs with [
    [x :: xs] ->  (f x) >>= fun x -> map_bind f [x :: acc] xs
  | [] -> Result.Ok (List.rev acc) ]
;

value safe_map f l = List.rev (List.rev_map f l) ;

value result_to_yojson pa pb = fun [
  Ok arg0 -> `List [`String "Ok"; pa arg0]
| Error arg0 -> `List [`String "Error"; pb arg0]
]
;

open Rresult.R ;
open Yojson ;
value result_of_yojson pa pb = fun [
  `List [`String "Ok"; arg0] ->
  (pa arg0) >>= (fun arg0 -> Result.Ok (Ok arg0))
| `List [`String "Error"; arg0] ->
  (pb arg0) >>= (fun arg0 -> Result.Ok (Error arg0))
| _ -> Result.Error "result"
]
;

(** These two copied and modified from Jane Street's sexplib0 *)
value hashtbl_to_yojson keymarsh valmarsh ht =
`List (Hashtbl.fold (fun k v acc -> [`List [keymarsh k; valmarsh v] :: acc]) ht [])
;

value hashtbl_of_yojson keydemarsh valdemarsh (json : Yojson.Safe.t) = match json with [
  `List lst ->
  let demarsh1 = fun [
    `List[k;v] ->
    (keydemarsh k) >>= (fun k -> (valdemarsh v) >>= (fun v -> Result.Ok (k,v)))
  | _ -> Result.Error "tuple list needed"
  ] in
  (List.fold_left (fun acc t ->
       acc >>= (fun acc -> (demarsh1 t) >>= (fun (k,v) -> Result.Ok [(k,v)::acc])))
      (Result.Ok[]) lst)
  >>= (fun l ->
      let ht = Hashtbl.create 5 in do {
        List.iter (fun (k,v) -> Hashtbl.add ht k v) l ;
        Result.Ok ht
      })
| _ -> Result.Error "list needed" ]
;

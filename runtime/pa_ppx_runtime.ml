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

value result_of_yojson pa pb = fun [
  `List [`String "Ok"; arg0] ->
  (pa arg0) >>= (fun arg0 -> Result.Ok (Ok arg0))
| `List [`String "Error"; arg0] ->
  (pb arg0) >>= (fun arg0 -> Result.Ok (Error arg0))
| _ -> Result.Error "result"
]
;

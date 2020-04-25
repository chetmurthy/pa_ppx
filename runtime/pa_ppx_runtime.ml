(* camlp5r *)
(* pa_ppx_runtime.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)


module Fmt = Fmt ;

value rec map_bind f acc xs =
  let open Rresult.R in 
  match xs with [
    [x :: xs] ->  (f x) >>= fun x -> map_bind f [x :: acc] xs
  | [] -> Result.Ok (List.rev acc) ]
;

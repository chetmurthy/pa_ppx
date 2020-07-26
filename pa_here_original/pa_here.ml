(* camlp5o *)
(* pa_here.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_passthru
open Ppxutil

let quote_position loc p =
  let open Lexing in
  <:expr< let open Lexing in {
  pos_fname = $str:p.pos_fname$ ;
  pos_lnum = $int:string_of_int p.pos_lnum$ ;
  pos_bol = $int:string_of_int p.pos_bol$ ;
  pos_cnum = $int:string_of_int p.pos_cnum$ } >>

let rewrite_expr arg = function
  <:expr:< [%here] >> ->
    let pos = start_position_of_loc loc in
    quote_position loc pos
| _ -> assert false


let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%here] >> as z ->
    fun arg fallback ->
      Some (rewrite_expr arg z)
  ] } in
  Pa_passthru.(install { name = "pa_here"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;

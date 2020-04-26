(* camlp5r *)
(* pa_here.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
#load "pa_macro.cmo";
#load "pa_macro_gram.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_passthru ;
open Ppxutil ;

value rewrite_expr arg = fun [
  <:expr:< [%here] >> ->
  let pos_fname = Ploc.file_name loc in
  let pos_lnum = string_of_int (Ploc.line_nb loc) in
  let pos_bol = string_of_int (Ploc.bol_pos loc) in
  let pos_cnum = string_of_int (Ploc.first_pos loc) in
  <:expr< let open Lexing in {
  pos_fname = $str:pos_fname$ ; pos_lnum = $int:pos_lnum$ ;
  pos_bol = $int:pos_bol$ ; pos_cnum = $int:pos_cnum$ } >>
| _ -> assert False
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%here] >> as z ->
    fun arg ->
      Some (rewrite_expr arg z)
  ] } in
  Pa_passthru.install ("pa_here", ef)
;

install();

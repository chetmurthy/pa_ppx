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
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;

value rewrite_expr arg = fun [
  <:expr:< [%here] >> ->
    let pos = start_position_of_loc loc in
    quote_position loc pos
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
  Pa_passthru.(install { name = "pa_here"; ef =  ef ; before = [] ; after = [] })
;

install();

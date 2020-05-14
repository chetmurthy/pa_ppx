(* camlp5r *)
(* pa_assert.ml,v *)
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
(* ?here:Lexing.position list -> ?message:string -> ?equal:(type -> type -> bool) -> expect:typ -> typ -> unit *)
  <:expr:< [%test_eq: $type:ty$ ] >> ->
    <:expr< fun ?{here} ?{message} ?{equal} x y ->
      let equal = match equal with [ Some f -> f | None -> [%eq: $type:ty$] ] in
      let pp () = [%show: $type:ty$] in
      if not (equal x y) then failwith (Printf.sprintf "comparison failed: %a vs %a" pp x pp y)
      else () >>

(* ?here:Lexing.position list -> ?message:string -> ?equal:(type -> type -> bool) -> expect:typ -> typ -> unit *)
| <:expr:< [%test_result: $type:ty$ ] >> ->
    <:expr< fun ?{here} ?{message} ?{equal} ~{expect=x} y ->
    let equal = match equal with [ Some f -> f | None -> [%eq: $type:ty$] ] in
      let pp () = [%show: $type:ty$] in
      if not (equal x y) then failwith (Printf.sprintf "expected %a but got %a" pp x pp y)
      else () >>

(* ?here:Lexing.position list -> ?message:string -> (type -> bool) -> type -> unit *)

| <:expr:< [%test_pred: $type:ty$ ] >> ->
    <:expr< fun ?{here} ?{message} pred y ->
    let pp () = [%show: $type:ty$] in
      if not (pred y) then failwith (Printf.sprintf "failed predicate: %a" pp y)
      else () >>

| _ -> assert False
]
;

value rewrite_ctyp arg = fun [
  <:ctyp:< [%test_eq: $type:ty$ ] >> ->
    <:ctyp< ?here:(list Lexing.position) -> ?message:string -> ?equal:($ty$ -> $ty$ -> bool) -> $ty$ -> $ty$ -> unit >>

| <:ctyp:< [%test_result: $type:ty$ ] >> ->
    <:ctyp< ?here:(list Lexing.position) -> ?message:string -> ?equal:($ty$ -> $ty$ -> bool) -> ~expect: $ty$ -> $ty$ -> unit >>

| <:ctyp:< [%test_pred: $type:ty$ ] >> ->
    <:ctyp< ?here:(list Lexing.position) -> ?message:string -> ($ty$ -> bool) -> $ty$ -> unit >>

| _ -> assert False
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%test_eq: $type:_$ ] >>
  | <:expr:< [%test_result: $type:_$ ] >>
  | <:expr:< [%test_pred: $type:_$ ] >>
    as z ->
    fun arg ->
      Some (rewrite_expr arg z)
  ] ;
            ctyp = extfun ef.ctyp with [
    <:ctyp:< [%test_eq: $type:_$ ] >>
  | <:ctyp:< [%test_result: $type:_$ ] >>
  | <:ctyp:< [%test_pred: $type:_$ ] >>
    as z ->
    fun arg ->
      Some (rewrite_ctyp arg z)
  ] } in
Pa_passthru.(install {
    name = "pa_assert";
    ef =  ef ;
    pass = None ;
    before = ["pa_deriving"] ;
    after = ["pa_inline_test"; "pa_expect_test"]
  })
;

install();

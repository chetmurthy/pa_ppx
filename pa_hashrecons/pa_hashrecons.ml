(* camlp5r *)
(* pa_hashrecons.ml,v *)
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

module Fresh = struct
  type t = (string * ref int) ;
  value mk s = (s, ref 0) ;

  value get (s, r) = do {
    let i = r.val in
      r.val := 1 + r.val ;
      Printf.sprintf "%s_%d" s i
  }
  ;
end
;

value rewrite_patt f p0 =
  let rec rerec = fun [
    <:patt:< $_$ $_$ >> as p ->
      let (p, args) = Patt.unapplist p in
      let args = List.map rerec args in
      let p = Patt.applist p args in
      let newv = Fresh.get f in
      <:patt< ( $p$ as $lid:newv$ ) >>
  | <:patt:< $uid:uid$ >> ->
      let newv = Fresh.get f in
      <:patt< ( $uid:uid$ as $lid:newv$ ) >>
  | p ->
    let loc = loc_of_patt p in
      let newv = Fresh.get f in
      <:patt< ( $p$ as $lid:newv$ ) >>
  ] in
  rerec p0
;

value rewrite_expr p e = e ;

value rewrite_case_branch arg = fun [
  (<:patt< $p$ [@hashrecons $lid:rootvar$ ; ] >>, eopt, body) ->
    let p = rewrite_patt (Fresh.mk rootvar) p in
    let body = rewrite_expr p body in
    (p, eopt, body)
| _ -> assert False
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
  case_branch = extfun ef.case_branch with [
    (<:patt< $_$ [@hashrecons $lid:_$ ; ] >>, _, _) as z ->
    fun arg ->
      Some (rewrite_case_branch arg z)
  ] } in
  Pa_passthru.install ("pa_hashrecons", ef)
;

install();

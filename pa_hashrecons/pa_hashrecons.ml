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

value rec rerec0 = fun [
    (<:patt< ( $p$ as $lid:z$ ) >>, e) ->
    rerec1 z (p, e)

  ]
  and rerec1 zvar = fun [
    (<:patt< ($p$ as $z$ ) >>, e) ->
    rerec1 zvar (p, e)

  | (<:patt< $uid:puid$ >>, (<:expr< $uid:euid$ >> as e)) when puid = euid ->
    let czvar = "c"^zvar in
    (czvar, e, zvar)

  | ((<:patt< $_$ $_$ >> as p), (<:expr:< $_$ $_$ >> as e)) ->
    let (p, pargs) = Patt.unapplist p in
    let (e, eargs) = Expr.unapplist e in
    match (p, e) with [
      (<:patt< $uid:puid$ >>, <:expr< $uid:euid$ >>)
      when puid = euid && List.length pargs = List.length eargs ->
      let cz_e_zs = List.map2 (fun p e -> rerec0 (p,e)) pargs eargs in
      let pred = List.fold_right (fun (czvar, _, zvar) rest ->
          <:expr< ($lid:czvar$ == $lid:zvar$) && $rest$ >>)
          cz_e_zs <:expr< True >> in
      let consexp = Expr.applist <:expr< $uid:euid$ >>
          (List.map (fun (cz, _, _) -> <:expr< $lid:cz$ >>) cz_e_zs) in
      let body = <:expr< if $pred$ then $lid:zvar$ else $consexp$ >> in
      let e = List.fold_right (fun (cz, e, _) rhs ->
          <:expr< let $lid:cz$ = $e$ in $rhs$ >>)
          cz_e_zs body in
      let czvar = "c"^zvar in
      (czvar, e, zvar)

    | _ -> assert False
    ]

  | (<:patt< $lid:pv$ >>, e) ->
    let czvar = "c"^zvar in
    (czvar, e, zvar)

  ]
;

value rewrite_expr0 (p0, e0) =
  let loc = loc_of_expr e0 in
  let (cz, e, _) = rerec0 (p0,e0) in
  <:expr< let $lid:cz$ = $e$ in $lid:cz$ >>
;

value rec rewrite_expr rv1 p e = match (p, e) with [
  (p, <:expr< $e$ [@hashrecons $lid:rv2$ ; ] >>) when rv1 = rv2 ->
  rewrite_expr0 (p, e)
| (p, <:expr:< let $_flag:r$ $_list:l$ in $x$ >>) ->
  let x = rewrite_expr rv1 p x in
  <:expr< let $_flag:r$ $_list:l$ in $x$ >>
| _ -> failwith "Pa_hashrecons.rewrite_expr: no expr found to match pattern"
]
;

value rewrite_case_branch arg = fun [
  (<:patt< $p$ [@hashrecons $lid:rootvar$ ; ] >>, eopt, body) ->
    let p = rewrite_patt (Fresh.mk rootvar) p in
    let body = rewrite_expr rootvar p body in
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
